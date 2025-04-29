package handler

import (
	"github.com/labstack/echo/v4"
	"github.com/minusno0708/vstreamer/internal/utils"
)

func StreamHandler(c echo.Context) error {
	videoID := c.Param("id")
	filename := c.Param("file")

	filePath := utils.ToVideoPath(videoID) + "/" + filename

	return c.File(filePath)
}
