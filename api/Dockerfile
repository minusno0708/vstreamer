FROM golang:1.24.2

WORKDIR /app

COPY . .

RUN apt-get update && \
    apt-get install -y ffmpeg && \
    go mod download

EXPOSE 1323

CMD ["go", "run", "cmd/vstreamer/main.go"]