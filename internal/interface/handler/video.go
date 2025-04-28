package handler

import (
	"net/http"

	"github.com/labstack/echo/v4"
	"github.com/minusno0708/vstreamer/internal/usecase"
)

type VideoHandlerInterface interface {
	GetVideosHandler(c echo.Context) error
	GetVideoHandler(c echo.Context) error
	UploadVideoHandler(c echo.Context) error
}

type videoHandler struct {
	VideoUsecase usecase.VideoUseCase
}

func NewVideoHandler(videoUsecase usecase.VideoUseCase) *videoHandler {
	return &videoHandler{
		VideoUsecase: videoUsecase,
	}
}

func (h *videoHandler) GetVideosHandler(c echo.Context) error {
	videoList, err := h.VideoUsecase.FindAll()
	if err != nil {
		return c.String(http.StatusInternalServerError, err.Error())
	}

	return c.JSON(http.StatusOK, videoList)
}

func (h *videoHandler) GetVideoHandler(c echo.Context) error {
	videoId := c.Param("id")
	video, err := h.VideoUsecase.FindByID(videoId)
	if err != nil {
		return c.String(http.StatusInternalServerError, err.Error())
	}
	return c.JSON(http.StatusOK, video)
}

func (h *videoHandler) UploadVideoHandler(c echo.Context) error {
	videoFile, err := c.FormFile("video")
	if err != nil {
		return c.String(http.StatusBadRequest, err.Error())
	}

	err = h.VideoUsecase.Save(videoFile)
	if err != nil {
		return c.String(http.StatusInternalServerError, err.Error())
	}

	return c.String(http.StatusOK, "upload video")
}
