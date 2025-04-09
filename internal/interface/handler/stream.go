package handler

import (
	"net/http"

	"github.com/labstack/echo/v4"
)

func StreamHandler(c echo.Context) error {
	path := c.Param("path")
	return c.String(http.StatusOK, "stream "+path+" video")
}
