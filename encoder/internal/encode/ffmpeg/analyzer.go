package ffmpeg

import (
	"encoding/json"
	"fmt"
	"os/exec"
)

type Analyzer struct {
	Streams []struct {
		Width  int `json:"width"`
		Height int `json:"height"`
		Tags   struct {
			Rotate string `json:"rotate"`
		} `json:"tags"`
	} `json:"streams"`
}

func NewAnalyzer(path string) (*Analyzer, error) {
	cmd := exec.Command(
		"ffprobe",
		"-v", "error",
		"-select_streams", "v:0",
		"-show_entries", "stream",
		"-of", "json",
		path,
	)

	out, err := cmd.Output()
	if err != nil {
		return nil, err
	}

	var result Analyzer
	if err := json.Unmarshal(out, &result); err != nil {
		return nil, err
	}

	if len(result.Streams) == 0 {
		return nil, fmt.Errorf("no video stream found in %s", path)
	}

	return &result, nil
}

func (a *Analyzer) GetResolution() string {
	if !a.IsRotated() {
		return fmt.Sprintf("%dx%d", a.Streams[0].Width, a.Streams[0].Height)
	} else {
		return fmt.Sprintf("%dx%d", a.Streams[0].Height, a.Streams[0].Width)
	}
}

func (a *Analyzer) IsRotated() bool {
	rotate := a.Streams[0].Tags.Rotate
	if rotate == "90" || rotate == "270" {
		return true
	}
	return false
}
