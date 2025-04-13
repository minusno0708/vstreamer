package handler

import (
	"io"
	"mime/multipart"
	"net/http"
	"os"
	"os/exec"
	"strings"

	"github.com/labstack/echo/v4"
)

const (
	videoDir = "videos/"
)

func toVideoPath(videoName string) string {
	return videoDir + videoName
}

func saveVideoFile(videoFile *multipart.FileHeader) error {
	src, err := videoFile.Open()
	if err != nil {
		return err
	}
	defer src.Close()

	videoPath := toVideoPath(videoFile.Filename)

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

func encodeVideo(videoId string) error {
	videoDir := toVideoPath(videoId)

	err := os.Mkdir(videoDir, 0755)
	if err != nil {
		return err
	}

	cmd := exec.Command(
		"ffmpeg",
		"-i", toVideoPath(videoId)+".mp4",
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
		toVideoPath(videoId)+"/video.mpd",
	)

	_, err = cmd.Output()
	if err != nil {
		return err
	}

	return nil
}

func VideoGetHandler(c echo.Context) error {
	pageName := c.Param("id")
	return c.String(http.StatusOK, "show "+pageName+" video")
}

func VideoUploadHandler(c echo.Context) error {
	videoFile, err := c.FormFile("video")
	if err != nil {
		return c.String(http.StatusBadRequest, err.Error())
	}

	err = saveVideoFile(videoFile)
	if err != nil {
		return c.String(http.StatusInternalServerError, err.Error())
	}

	videoId := strings.Split(videoFile.Filename, ".")[0]
	err = encodeVideo(videoId)
	if err != nil {
		return c.String(http.StatusInternalServerError, err.Error())
	}

	err = os.Remove(toVideoPath(videoFile.Filename))
	if err != nil {
		return c.String(http.StatusInternalServerError, err.Error())
	}

	return c.String(http.StatusOK, "upload video")
}
