package encode

import (
	"testing"

	"github.com/minusno0708/vstreamer/encoder/internal/file"
)

const (
	homeDir = "../../../"
)

func TestEncode(t *testing.T) {
	videoName := "sample1"
	originalPath := homeDir + "contents/videos/test/" + videoName + ".mp4"
	encodeDir := homeDir + "contents/videos/encoded/" + videoName

	isExist := file.IsExist(originalPath)
	if !isExist {
		t.Errorf("File does not exist: %s", originalPath)
		return
	}

	err := file.CreateDir(encodeDir)
	if err != nil {
		t.Errorf("Failed to create directory: %v", err)
		return
	}

	err = Encode(originalPath, encodeDir)
	if err != nil {
		t.Errorf("Encode failed: %v", err)
	}
}
