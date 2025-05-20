package main

import (
	"fmt"
	"log"
	"os"

	"github.com/minusno0708/vstreamer/encoder/internal/rabbitmq"
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
	}
}
