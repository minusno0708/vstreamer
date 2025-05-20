package main

import (
	"fmt"
	"log"
	"os"
	"time"

	"github.com/minusno0708/vstreamer/encoder/internal/encode"
	"github.com/minusno0708/vstreamer/encoder/internal/file"
	"github.com/minusno0708/vstreamer/encoder/internal/rabbitmq"
)

const (
	maxRetries = 10
	retryDelay = 5 * time.Second
)

func main() {
	log.Println("Starting video encoder...")

	mqUser := os.Getenv("MQ_USER")
	mqPass := os.Getenv("MQ_PASS")
	mqHost := os.Getenv("MQ_HOST")
	mqPort := os.Getenv("MQ_PORT")
	queueName := os.Getenv("QUEUE_NAME")

	dsn := fmt.Sprintf("amqp://%s:%s@%s:%s/", mqUser, mqPass, mqHost, mqPort)

	conn, err := rabbitmq.NewConnection(dsn)
	if err != nil {
		log.Panicf("Failed to connect to RabbitMQ: %v\n", err)
		return
	}
	defer conn.Close()

	ch, err := rabbitmq.NewChannel(conn)
	if err != nil {
		log.Panicf("Failed to open a channel: %v\n", err)
		return
	}
	defer ch.Close()

	msgs, err := rabbitmq.Receive(ch, queueName)
	if err != nil {
		log.Panicf("Failed to receive message: %v\n", err)
		return
	}

	for msg := range msgs {
		log.Printf("Received message: %s\n", msg.Body)

		videoName := string(msg.Body)
		originalPath := file.ToOriginalPath(videoName, "mp4")
		isExist := file.IsExist(originalPath)
		if !isExist {
			log.Printf("File does not exist: %s\n", originalPath)
			continue
		}

		videoDir := file.ToEncodePath(videoName)
		err := file.CreateDir(videoDir)
		if err != nil {
			log.Printf("Failed to create directory: %v\n", err)
			continue
		}

		err = encode.Encode(originalPath, videoDir)
		if err != nil {
			log.Printf("Failed to encode video: %v\n", err)
			continue
		}

		err = file.RemoveFile(originalPath)
		if err != nil {
			log.Printf("Failed to remove file: %v\n", err)
			continue
		}
		log.Printf("Successfully encoded video: %s\n", videoName)
	}
}
