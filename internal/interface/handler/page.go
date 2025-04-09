package handler

import (
	"github.com/labstack/echo/v4"
)

func PageHandler(c echo.Context) error {
	pageName := c.Param("name")

	return c.File("pages/" + pageName + ".html")
}
