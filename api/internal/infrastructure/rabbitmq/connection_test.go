package rabbitmq

import "testing"

func TestNewConnection(t *testing.T) {
	conn, err := NewConnection("amqp://guest:guest@localhost:5672/")
	if err != nil {
		t.Fatalf("Failed to connect to RabbitMQ: %v", err)
	}
	defer conn.Close()
}
