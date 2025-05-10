package rabbitmq

import (
	amqp "github.com/rabbitmq/amqp091-go"
)

func NewConnection(dsn string) (*amqp.Connection, error) {
	conn, err := amqp.Dial(dsn)
	if err != nil {
		return nil, err
	}

	return conn, nil
}

func NewChannel(conn *amqp.Connection) (*amqp.Channel, error) {
	ch, err := conn.Channel()
	if err != nil {
		return nil, err
	}

	return ch, nil
}
