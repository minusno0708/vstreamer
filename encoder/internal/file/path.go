package file

const (
	baseVideoDir = "contents/video/original/"
	encodeDir    = "contents/video/encoded/"
)

func ToBaseVideoPath(name, videoType string) string {
	return baseVideoDir + name + "." + videoType
}

func ToEncodePath(name string) string {
	return encodeDir + name
}
