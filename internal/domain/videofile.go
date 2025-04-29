package domain

type VideoFile struct {
	Name     string
	Contents []byte
	MimeType string
}

func NewVideoFile(name string, contents []byte, mimeType string) *VideoFile {
	return &VideoFile{
		Name:     name,
		Contents: contents,
		MimeType: mimeType,
	}
}

func (vf *VideoFile) UpdateName(newName string) {
	vf.Name = newName
}
