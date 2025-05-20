package utils

const (
	videoDir = "/data/videos/"
)

func ToVideoPath(videoID string) string {
	return videoDir + videoID
}
