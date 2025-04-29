package repository

import (
	"bytes"
	"fmt"
	"io"
	"os"

	"github.com/minusno0708/vstreamer/internal/domain"
	"github.com/minusno0708/vstreamer/internal/utils"
)

type VideoFileRepository interface {
	Save(file *domain.VideoFile) error
	Remove(file *domain.VideoFile) error
}

type videoFileRepository struct{}

func NewVideoFileRepository() *videoFileRepository {
	return &videoFileRepository{}
}

func (r *videoFileRepository) Save(file *domain.VideoFile) error {
	videoPath := utils.ToVideoPath(file.Name) + ".mp4"

	dst, err := os.Create(videoPath)
	if err != nil {
		return err
	}
	defer dst.Close()

	reader := bytes.NewReader(file.Contents)
	if _, err = io.Copy(dst, reader); err != nil {
		return err
	}

	return nil
}

func (r *videoFileRepository) Remove(file *domain.VideoFile) error {
	videoPath := utils.ToVideoPath(file.Name) + ".mp4"

	if err := os.Remove(videoPath); err != nil {
		return fmt.Errorf("failed to remove file: %w", err)
	}

	return nil
}
