package handler

import (
	"net/http"

	"github.com/labstack/echo/v4"
)

func PageHandler(c echo.Context) error {
	pageName := c.Param("name")
	return c.String(http.StatusOK, "show "+pageName+" page")
}
