package repository

import (
	"testing"

	"github.com/minusno0708/vstreamer/internal/infrastructure/rabbitmq"
)

func TestSend(t *testing.T) {
	conn, err := rabbitmq.NewConnection("amqp://guest:guest@localhost:5672/")
	if err != nil {
		t.Fatalf("Failed to connect to RabbitMQ: %v", err)
	}
	defer conn.Close()

	ch, err := rabbitmq.NewChannel(conn)
	if err != nil {
		t.Fatalf("Failed to open a channel: %v", err)
	}
	defer ch.Close()

	encoderRepository, err := NewRabbitMqRepository(ch, "video-encoder")
	if err != nil {
		t.Fatalf("Failed to create encoder repository: %v", err)
	}

	err = encoderRepository.Send("testvideo")
	if err != nil {
		t.Fatalf("Failed to send video file: %v", err)
	}
}
