package utils

const (
	videoDir = "../videos/"
	pageDir  = "../pages/"
)

func ToVideoPath(videoID string) string {
	return videoDir + videoID
}

func ToPagePath(pageName string) string {
	return pageDir + pageName + ".html"
}
