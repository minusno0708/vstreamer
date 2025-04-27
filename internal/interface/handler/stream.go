package handler

import (
	"github.com/labstack/echo/v4"
)

func StreamHandler(c echo.Context) error {
	videoId := c.Param("id")
	filename := c.Param("file")

	filePath := toVideoPath(videoId) + "/" + filename

	return c.File(filePath)
}
