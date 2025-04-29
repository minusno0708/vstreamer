package repository

import (
	"bytes"
	"io"
	"os"

	"github.com/minusno0708/vstreamer/internal/domain"
	"github.com/minusno0708/vstreamer/internal/utils"
)

type VideoFileRepository interface {
	Save(file *domain.VideoFile) error
}

type videoFileRepository struct{}

func NewVideoFileRepository() *videoFileRepository {
	return &videoFileRepository{}
}

func (r *videoFileRepository) Save(videoFile *domain.VideoFile) error {
	videoPath := utils.ToVideoPath(videoFile.Name) + ".mp4"

	dst, err := os.Create(videoPath)
	if err != nil {
		return err
	}
	defer dst.Close()

	reader := bytes.NewReader(videoFile.Contents)
	if _, err = io.Copy(dst, reader); err != nil {
		return err
	}

	return nil
}
