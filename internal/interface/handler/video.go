package handler

import (
	"io"
	"mime/multipart"
	"net/http"
	"os"

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

	dst, err := os.Create(toVideoPath(videoFile.Filename))
	if err != nil {
		return err
	}
	defer dst.Close()

	if _, err = io.Copy(dst, src); err != nil {
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
		return c.String(http.StatusBadRequest, "video not found")
	}

	if err := saveVideoFile(videoFile); err != nil {
		return c.String(http.StatusInternalServerError, "failed to save video")
	}

	return c.String(http.StatusOK, "upload video")
}
