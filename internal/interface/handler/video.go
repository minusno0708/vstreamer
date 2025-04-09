package handler

import (
	"net/http"

	"github.com/labstack/echo/v4"
)

func VideoGetHandler(c echo.Context) error {
	pageName := c.Param("id")
	return c.String(http.StatusOK, "show "+pageName+" video")
}

func VideoUploadHandler(c echo.Context) error {
	return c.String(http.StatusOK, "upload video")
}
