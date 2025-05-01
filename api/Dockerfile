FROM golang:1.24.2

WORKDIR /app

COPY . .
COPY ../pages /app/pages
COPY ../videos /app/videos


RUN apt-get update && \
    apt-get install -y ffmpeg && \
    go mod download

EXPOSE 1323

CMD ["go", "run", "cmd/vstreamer/main.go"]