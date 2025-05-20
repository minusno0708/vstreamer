package encode

import (
	"os/exec"
)

func Encode(basePath, encodeDir string) error {
	cmd := exec.Command(
		"ffmpeg",
		"-i", basePath,
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
		encodeDir+"/manifest.mpd",
	)

	_, err := cmd.Output()
	if err != nil {
		return err
	}

	return nil
}
