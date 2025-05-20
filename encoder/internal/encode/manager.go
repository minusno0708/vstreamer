package encode

import (
	"github.com/minusno0708/vstreamer/encoder/internal/encode/ffmpeg"
)

func Encode(originalPath, encodeDir string) error {
	cmdBuilder := ffmpeg.NewCmdBuilder(originalPath, encodeDir)

	cmd := cmdBuilder.Build()

	_, err := cmd.Output()
	if err != nil {
		return err
	}

	return nil
}
