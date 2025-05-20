package file

const (
	originalDir = "/data/videos/original/"
	encodeDir   = "/data/videos/encoded/"
)

func ToOriginalPath(name, videoType string) string {
	return originalDir + name + "." + videoType
}

func ToEncodePath(name string) string {
	return encodeDir + name
}
