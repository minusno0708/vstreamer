package ffmpeg

import "os/exec"

type CmdBuilder struct {
	basefilePath string
	encodeDir    string
}

func NewCmdBuilder(basefilePath, encodeDir string) *CmdBuilder {
	return &CmdBuilder{
		basefilePath: basefilePath,
		encodeDir:    encodeDir,
	}
}

func (c *CmdBuilder) Build() *exec.Cmd {
	return exec.Command(
		"ffmpeg",
		"-i", c.basefilePath,
		"-c:v", "libx264",
		"-b:v", "1M",
		"-s", "1280x720",
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
