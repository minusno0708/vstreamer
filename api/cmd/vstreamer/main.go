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
	"github.com/minusno0708/vstreamer/internal/infrastructure/rabbitmq"
	mqrepo "github.com/minusno0708/vstreamer/internal/infrastructure/rabbitmq/repository"
	"github.com/minusno0708/vstreamer/internal/interface/handler"
	"github.com/minusno0708/vstreamer/internal/usecase"
)

func main() {
	dbUser := os.Getenv("DB_USER")
	dbPass := os.Getenv("DB_PASS")
	dbHost := os.Getenv("DB_HOST")
	dbPort := os.Getenv("DB_PORT")
	dbName := os.Getenv("DB_NAME")

	dbDsn := fmt.Sprintf("%s:%s@tcp(%s:%s)/%s", dbUser, dbPass, dbHost, dbPort, dbName)

	mqUser := os.Getenv("MQ_USER")
	mqPass := os.Getenv("MQ_PASS")
	mqHost := os.Getenv("MQ_HOST")
	mqPort := os.Getenv("MQ_PORT")
	queueName := os.Getenv("QUEUE_NAME")

	mqDsn := fmt.Sprintf("amqp://%s:%s@%s:%s/", mqUser, mqPass, mqHost, mqPort)

	db, err := mysql.NewConnection(dbDsn)
	if err != nil {
		panic(err)
	}
	defer db.Close()

	conn, err := rabbitmq.NewConnection(mqDsn)
	if err != nil {
		panic(err)
	}
	defer conn.Close()

	ch, err := rabbitmq.NewChannel(conn)
	if err != nil {
		panic(err)
	}
	defer ch.Close()

	videoFileRepo := filerepo.NewVideoFileRepository()
	videoRepo := mysqlrepo.NewVideoRepository(db)
	mqRepository, err := mqrepo.NewRabbitMqRepository(ch, queueName)
	if err != nil {
		panic(err)
	}

	videoUsecase := usecase.NewVideoUseCase(videoRepo, videoFileRepo, mqRepository)
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
