package main

import (
	"net/http"

	"github.com/labstack/echo/v4"
	"github.com/labstack/echo/v4/middleware"

	filerepo "github.com/minusno0708/vstreamer/internal/infrastructure/filesystem/repository"
	"github.com/minusno0708/vstreamer/internal/infrastructure/mysql"
	mysqlrepo "github.com/minusno0708/vstreamer/internal/infrastructure/mysql/repository"
	"github.com/minusno0708/vstreamer/internal/interface/handler"
	"github.com/minusno0708/vstreamer/internal/usecase"
)

func main() {
	db, err := mysql.NewConnection()
	if err != nil {
		panic(err)
	}
	defer db.Close()

	videoFileRepo := filerepo.NewVideoFileRepository()

	videoRepo := mysqlrepo.NewVideoRepository(db)
	videoUsecase := usecase.NewVideoUseCase(videoRepo, videoFileRepo)
	videoHandler := handler.NewVideoHandler(videoUsecase)

	e := echo.New()

	e.Use(middleware.Logger())

	e.GET("/", func(c echo.Context) error {
		return c.String(http.StatusOK, "Hello, World!")
	})

	e.GET("/videos", videoHandler.GetVideosHandler)
	e.POST("/videos", videoHandler.UploadVideoHandler)

	e.GET("/videos/:id", videoHandler.GetVideoHandler)

	e.Logger.Fatal(e.Start(":1323"))
}
