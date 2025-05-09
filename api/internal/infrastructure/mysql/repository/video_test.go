package repository

import (
	"testing"

	"github.com/minusno0708/vstreamer/internal/domain"
	"github.com/minusno0708/vstreamer/internal/infrastructure/mysql"
)

var testVideo = &domain.Video{
	ID:   "test_id",
	Name: "test_name",
}

func TestSave(t *testing.T) {
	db, err := mysql.NewConnection("root:root@tcp(localhost:3306)/contents_db")
	if err != nil {
		t.Fatalf("Failed to connect to database: %v", err)
	}
	defer db.Close()
	videoRepo := NewVideoRepository(db)
	video := domain.NewVideo(testVideo.Name)

	videoID, err := videoRepo.Save(video)
	if err != nil {
		t.Fatalf("Failed to save video: %v", err)
	}
	testVideo.ID = videoID
}

func TestFindByID(t *testing.T) {
	db, err := mysql.NewConnection("root:root@tcp(localhost:3306)/contents_db")
	if err != nil {
		t.Fatalf("Failed to connect to database: %v", err)
	}
	defer db.Close()
	videoRepo := NewVideoRepository(db)
	video, err := videoRepo.FindByID(testVideo.ID)
	if err != nil {
		t.Fatalf("Failed to find video by ID: %v", err)
	}
	if video == nil {
		t.Fatal("Expected to find a video, but got nil")
	}
	if video.ID != testVideo.ID {
		t.Fatalf("Expected video ID %s, but got %s", testVideo.ID, video.ID)
	}
	if video.Name != testVideo.Name {
		t.Fatalf("Expected video name %s, but got %s", testVideo.Name, video.Name)
	}
}

func TestFindByName(t *testing.T) {
	db, err := mysql.NewConnection("root:root@tcp(localhost:3306)/contents_db")
	if err != nil {
		t.Fatalf("Failed to connect to database: %v", err)
	}
	defer db.Close()
	videoRepo := NewVideoRepository(db)
	video, err := videoRepo.FindByName(testVideo.Name)
	if err != nil {
		t.Fatalf("Failed to find video by name: %v", err)
	}
	if video == nil {
		t.Fatal("Expected to find a video, but got nil")
	}
	if video.Name != testVideo.Name {
		t.Fatalf("Expected video name %s, but got %s", testVideo.Name, video.Name)
	}
}

func TestFindAll(t *testing.T) {
	db, err := mysql.NewConnection("root:root@tcp(localhost:3306)/contents_db")
	if err != nil {
		t.Fatalf("Failed to connect to database: %v", err)
	}
	defer db.Close()
	videoRepo := NewVideoRepository(db)
	videos, err := videoRepo.FindAll()
	if err != nil {
		t.Fatalf("Failed to find all videos: %v", err)
	}
	if len(videos) == 0 {
		t.Fatal("Expected to find videos, but got none")
	}
}
