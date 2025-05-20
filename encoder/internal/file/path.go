package file

const (
	originalDir = "contents/video/original/"
	encodeDir   = "contents/video/encoded/"
)

func ToOriginalPath(name, videoType string) string {
	return originalDir + name + "." + videoType
}

func ToEncodePath(name string) string {
	return encodeDir + name
}
