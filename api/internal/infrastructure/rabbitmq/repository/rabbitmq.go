package repository

import (
	"context"
	"time"

	amqp "github.com/rabbitmq/amqp091-go"
)

type RabbitMqRepository interface {
	Send(msg string) error
}

type rabbitMqRepository struct {
	ch        *amqp.Channel
	queueName string
}

func NewRabbitMqRepository(ch *amqp.Channel, queueName string) (*rabbitMqRepository, error) {
	_, err := ch.QueueDeclare(
		queueName,
		false,
		false,
		false,
		false,
		nil,
	)
	if err != nil {
		return nil, err
	}

	return &rabbitMqRepository{
		ch:        ch,
		queueName: queueName,
	}, nil
}

func (r *rabbitMqRepository) Send(msg string) error {
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()

	err := r.ch.PublishWithContext(
		ctx,
		"",
		r.queueName,
		false,
		false,
		amqp.Publishing{
			ContentType: "text/plain",
			Body:        []byte(msg),
		},
	)
	if err != nil {
		return err
	}

	return nil
}
