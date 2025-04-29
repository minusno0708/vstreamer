package domain

import (
	"github.com/google/uuid"
)

type Video struct {
	ID   string `json:"id"`
	Name string `json:"name"`
}

func generateID() string {
	return uuid.New().String()
}

func NewVideo(name string) *Video {
	video := &Video{
		ID:   generateID(),
		Name: name,
	}

	return video
}
