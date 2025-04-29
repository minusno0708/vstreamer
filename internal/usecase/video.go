package usecase

import (
	"os"
	"os/exec"

	"github.com/minusno0708/vstreamer/internal/domain"
	filerepo "github.com/minusno0708/vstreamer/internal/infrastructure/filesystem/repository"
	mysqlrepo "github.com/minusno0708/vstreamer/internal/infrastructure/mysql/repository"
	"github.com/minusno0708/vstreamer/internal/utils"
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
}

func NewVideoUseCase(videoRepo mysqlrepo.VideoRepository, videoFileRepo filerepo.VideoFileRepository) VideoUseCase {
	return &videoUseCase{
		videoRepo:     videoRepo,
		videoFileRepo: videoFileRepo,
	}
}

func encodeVideo(videoID string) error {
	videoDir := utils.ToVideoPath(videoID)

	err := os.Mkdir(videoDir, 0755)
	if err != nil {
		return err
	}

	cmd := exec.Command(
		"ffmpeg",
		"-i", videoDir+".mp4",
		"-c:v", "libx264",
		"-b:v", "1M",
		"-s", "1280x720",
		"-keyint_min", "150",
		"-g", "150",
		"-profile:v", "high",
		"-preset", "medium",
		"-c:a", "aac",
		"-ac", "2",
		"-b:a", "128k",
		"-f", "dash",
		videoDir+"/manifest.mpd",
	)

	_, err = cmd.Output()
	if err != nil {
		return err
	}

	return nil
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
		encodeVideo(videoID)
		u.videoFileRepo.Remove(videoFile)
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
