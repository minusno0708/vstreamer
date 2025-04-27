package handler

import (
	"os"

	"github.com/labstack/echo/v4"
)

const (
	pagesDir = "pages/"
)

func toPagePath(pageName string) string {
	return pagesDir + pageName + ".html"
}

func isPageExist(path string) bool {
	_, err := os.Stat(path)
	return err == nil
}

func PageHandler(c echo.Context) error {
	pageName := c.Param("name")

	pagePath := toPagePath(pageName)

	if !isPageExist(pagePath) {
		pagePath = toPagePath("404")
	}

	return c.File(pagePath)
}
