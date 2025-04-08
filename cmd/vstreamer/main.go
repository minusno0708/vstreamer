package main

import (
	"net/http"

	"github.com/labstack/echo/v4"
)

func main() {
	e := echo.New()

	e.GET("/", func(c echo.Context) error {
		return c.String(http.StatusOK, "Hello, World!")
	})

	e.GET("pages/:name", func(c echo.Context) error {
		pageName := c.Param("name")
		return c.String(http.StatusOK, "show "+pageName+" page")
	})

	e.GET("streams/:path", func(c echo.Context) error {
		path := c.Param("path")
		return c.String(http.StatusOK, "stream "+path+" video")
	})

	e.GET("videos/:id", func(c echo.Context) error {
		pageName := c.Param("id")
		return c.String(http.StatusOK, "show "+pageName+" video")
	})

	e.POST("upload", func(c echo.Context) error {
		return c.String(http.StatusOK, "upload video")
	})

	e.Logger.Fatal(e.Start(":8080"))
}
