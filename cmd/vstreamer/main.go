package main

import (
	"net/http"

	"github.com/labstack/echo/v4"

	"github.com/minusno0708/vstreamer/internal/infrastructure/mysql"
	"github.com/minusno0708/vstreamer/internal/interface/handler"
)

func main() {
	db, err := mysql.NewConnection()
	if err != nil {
		panic(err)
	}
	defer db.Close()

	e := echo.New()

	e.GET("/", func(c echo.Context) error {
		return c.String(http.StatusOK, "Hello, World!")
	})

	e.GET("/pages", func(c echo.Context) error {
		return c.Redirect(http.StatusFound, "/pages/index")
	})

	e.GET("/pages/:name", handler.PageHandler)
	e.GET("/pages/videos/:id", handler.VideoPageHandler)

	e.GET("/streams/:id/:file", handler.StreamHandler)

	e.GET("/videos", handler.GetVideoListHandler)
	e.GET("/videos/:id", handler.GetVideoHandler)

	e.POST("/upload", handler.UploadVideoHandler)

	e.Logger.Fatal(e.Start(":8080"))
}
