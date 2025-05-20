package file

const (
	baseVideoDir = "videos/"
	encodeDir    = "videos/"
)

func ToBaseVideoPath(name, videoType string) string {
	return baseVideoDir + name + "." + videoType
}

func ToEncodePath(name string) string {
	return encodeDir + name
}
