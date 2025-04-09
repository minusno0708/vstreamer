package main

import (
	"net/http"

	"github.com/labstack/echo/v4"

	"github.com/minusno0708/vstreamer/internal/interface/handler"
)

func main() {
	e := echo.New()

	e.GET("/", func(c echo.Context) error {
		return c.String(http.StatusOK, "Hello, World!")
	})

	e.GET("/pages/:name", handler.PageHandler)

	e.GET("/streams/:path", handler.StreamHandler)

	e.GET("/videos/:id", handler.VideoGetHandler)

	e.POST("/upload", handler.VideoUploadHandler)

	e.Logger.Fatal(e.Start(":8080"))
}
