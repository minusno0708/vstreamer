package handler

import (
	"os"

	"github.com/labstack/echo/v4"
	"github.com/minusno0708/vstreamer/internal/utils"
)

func isPageExist(path string) bool {
	_, err := os.Stat(path)
	return err == nil
}

func PageHandler(c echo.Context) error {
	pageName := c.Param("name")

	pagePath := utils.ToPagePath(pageName)

	if !isPageExist(pagePath) {
		pagePath = utils.ToPagePath("404")
	}

	return c.File(pagePath)
}

func VideoPageHandler(c echo.Context) error {
	pagePath := utils.ToPagePath("video")

	return c.File(pagePath)
}
