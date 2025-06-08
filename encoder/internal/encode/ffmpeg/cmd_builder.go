package ffmpeg

import (
	"os/exec"
)

type CmdOption struct {
	key   string
	value string
}

type CmdBuilder struct {
	basefilePath string
	encodeDir    string
	size         CmdOption
}

func NewOption(key, value string) CmdOption {
	return CmdOption{
		key:   key,
		value: value,
	}
}

func NewCmdBuilder(basefilePath, encodeDir string) (*CmdBuilder, error) {
	cmdBuilder := &CmdBuilder{}

	cmdBuilder.basefilePath = basefilePath
	cmdBuilder.encodeDir = encodeDir

	analyzer, err := NewAnalyzer(basefilePath)
	if err != nil {
		return nil, err
	}

	resolution := analyzer.GetResolution()
	cmdBuilder.size = NewOption("-s", resolution)

	return cmdBuilder, nil
}

func (c *CmdBuilder) Build() *exec.Cmd {
	return exec.Command(
		"ffmpeg",
		"-i", c.basefilePath,
		"-c:v", "libx264",
		"-b:v", "1M",
		c.size.key, c.size.value,
		"-keyint_min", "150",
		"-g", "150",
		"-profile:v", "high",
		"-preset", "medium",
		"-c:a", "aac",
		"-ac", "2",
		"-b:a", "128k",
		"-f", "dash",
		c.encodeDir+"/manifest.mpd",
	)
}
