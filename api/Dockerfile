FROM golang:1.24.2

WORKDIR /app

COPY . .

RUN apt-get update && \
    apt-get install -y ffmpeg && \
    go install github.com/air-verse/air@latest && \
    go mod download

EXPOSE 1323

CMD ["air", "-c", ".air.toml"]