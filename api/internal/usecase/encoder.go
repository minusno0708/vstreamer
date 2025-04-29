package usecase

import (
	"os"
	"os/exec"

	"github.com/minusno0708/vstreamer/internal/utils"
)

func encodeVideo(videoID string) error {
	videoDir := utils.ToVideoPath(videoID)

	err := os.Mkdir(videoDir, 0755)
	if err != nil {
		return err
	}

	cmd := exec.Command(
		"ffmpeg",
		"-i", videoDir+".mp4",
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
		videoDir+"/manifest.mpd",
	)

	_, err = cmd.Output()
	if err != nil {
		return err
	}

	return nil
}
