package repository

import (
	"context"
	"time"

	amqp "github.com/rabbitmq/amqp091-go"

	"github.com/minusno0708/vstreamer/internal/domain"
)

type EncoderRepository interface {
	Send(file *domain.VideoFile) error
}

type encoderRepository struct {
	ch *amqp.Channel
}

const (
	queueName = "video-encoder"
)

func NewEncoderRepository(ch *amqp.Channel) (*encoderRepository, error) {
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

	return &encoderRepository{
		ch: ch,
	}, nil
}

func (r *encoderRepository) Send(file *domain.VideoFile) error {
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()

	err := r.ch.PublishWithContext(
		ctx,
		"",
		queueName,
		false,
		false,
		amqp.Publishing{
			ContentType: "text/plain",
			Body:        []byte(file.Name),
		},
	)
	if err != nil {
		return err
	}

	return nil
}
