package main

import (
	"fmt"
	"net/http"
	"os"

	"github.com/labstack/echo/v4"
	"github.com/labstack/echo/v4/middleware"

	filerepo "github.com/minusno0708/vstreamer/internal/infrastructure/filesystem/repository"
	"github.com/minusno0708/vstreamer/internal/infrastructure/mysql"
	mysqlrepo "github.com/minusno0708/vstreamer/internal/infrastructure/mysql/repository"
	"github.com/minusno0708/vstreamer/internal/interface/handler"
	"github.com/minusno0708/vstreamer/internal/usecase"
)

func main() {
	db_user := os.Getenv("DB_USER")
	db_pass := os.Getenv("DB_PASS")
	db_host := os.Getenv("DB_HOST")
	db_port := os.Getenv("DB_PORT")
	db_name := os.Getenv("DB_NAME")

	dsn := fmt.Sprintf("%s:%s@tcp(%s:%s)/%s", db_user, db_pass, db_host, db_port, db_name)

	db, err := mysql.NewConnection(dsn)
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
		return c.String(http.StatusOK, "vstreamer API")
	})

	e.GET("/videos", videoHandler.GetVideosHandler)
	e.POST("/videos", videoHandler.UploadVideoHandler)

	e.GET("/videos/:id", videoHandler.GetVideoHandler)

	e.Logger.Fatal(e.Start(":1323"))
}
