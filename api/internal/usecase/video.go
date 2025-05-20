package usecase

import (
	"github.com/labstack/gommon/log"
	"github.com/minusno0708/vstreamer/internal/domain"
	filerepo "github.com/minusno0708/vstreamer/internal/infrastructure/filesystem/repository"
	mysqlrepo "github.com/minusno0708/vstreamer/internal/infrastructure/mysql/repository"
	mqrepo "github.com/minusno0708/vstreamer/internal/infrastructure/rabbitmq/repository"
)

type VideoUseCase interface {
	Save(videoFile *domain.VideoFile) error
	FindByID(id string) (*domain.Video, error)
	FindByName(name string) (*domain.Video, error)
	FindAll() ([]*domain.Video, error)
}

type videoUseCase struct {
	videoRepo     mysqlrepo.VideoRepository
	videoFileRepo filerepo.VideoFileRepository
	mqRepo        mqrepo.RabbitMqRepository
}

func NewVideoUseCase(videoRepo mysqlrepo.VideoRepository, videoFileRepo filerepo.VideoFileRepository, mqRepo mqrepo.RabbitMqRepository) VideoUseCase {
	return &videoUseCase{
		videoRepo:     videoRepo,
		videoFileRepo: videoFileRepo,
		mqRepo:        mqRepo,
	}
}

func (u *videoUseCase) Save(videoFile *domain.VideoFile) error {
	video := domain.NewVideo(videoFile.Name)
	videoID, err := u.videoRepo.Save(video)
	if err != nil {
		return err
	}

	videoFile.UpdateName(videoID)

	err = u.videoFileRepo.Save(videoFile)
	if err != nil {
		return err
	}

	go func() {
		err := u.mqRepo.Send(videoFile.Name)
		if err != nil {
			log.Error("Failed to send video file to RabbitMQ: ", err)
			return
		}
	}()

	return nil
}

func (u *videoUseCase) FindByID(id string) (*domain.Video, error) {
	video, err := u.videoRepo.FindByID(id)
	if err != nil {
		return nil, err
	}
	return video, nil
}

func (u *videoUseCase) FindByName(name string) (*domain.Video, error) {
	video, err := u.videoRepo.FindByName(name)
	if err != nil {
		return nil, err
	}
	return video, nil
}

func (u *videoUseCase) FindAll() ([]*domain.Video, error) {
	videos, err := u.videoRepo.FindAll()
	if err != nil {
		return nil, err
	}
	return videos, nil
}
