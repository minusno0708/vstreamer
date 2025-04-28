package usecase

import (
	"io"
	"mime/multipart"
	"os"
	"os/exec"

	"github.com/minusno0708/vstreamer/internal/domain"
	"github.com/minusno0708/vstreamer/internal/infrastructure/mysql/repository"
	"github.com/minusno0708/vstreamer/internal/utils"
)

type VideoUseCase interface {
	Save(videoFile *multipart.FileHeader) error
	FindByID(id string) (*domain.Video, error)
	FindByName(name string) (*domain.Video, error)
	FindAll() ([]*domain.Video, error)
}

type videoUseCase struct {
	videoRepo repository.VideoRepository
}

func NewVideoUseCase(videoRepo repository.VideoRepository) VideoUseCase {
	return &videoUseCase{
		videoRepo: videoRepo,
	}
}

func saveVideoFile(videoFile *multipart.FileHeader, videoID string) error {
	src, err := videoFile.Open()
	if err != nil {
		return err
	}
	defer src.Close()

	videoPath := utils.ToVideoPath(videoID) + ".mp4"

	dst, err := os.Create(videoPath)
	if err != nil {
		return err
	}
	defer dst.Close()

	if _, err = io.Copy(dst, src); err != nil {
		return err
	}

	return nil
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

	err = os.Remove(utils.ToVideoPath(videoID) + ".mp4")
	if err != nil {
		return err
	}

	return nil
}

func (u *videoUseCase) Save(videoFile *multipart.FileHeader) error {
	video := domain.NewVideo(videoFile.Filename)
	videoID, err := u.videoRepo.Save(video)
	if err != nil {
		return err
	}

	err = saveVideoFile(videoFile, videoID)
	if err != nil {
		return err
	}

	go encodeVideo(videoID)

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
