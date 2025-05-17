package main

import (
	"log"

	"github.com/minusno0708/vstreamer/encoder/internal/rabbitmq"
)

func main() {
	log.Println("Starting video encoder...")

	conn, err := rabbitmq.NewConnection("amqp://guest:guest@localhost:5672/")
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

	msgs, err := rabbitmq.Receive(ch, "video-encoder")
	if err != nil {
		log.Panicf("Failed to receive message: %v\n", err)
		return
	}

	for msg := range msgs {
		log.Printf("Received message: %s\n", msg.Body)
	}
}
