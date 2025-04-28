package repository

import (
	"database/sql"

	"github.com/minusno0708/vstreamer/internal/domain"
)

type VideoRepository struct {
	db *sql.DB
}

func NewVideoRepository(db *sql.DB) *VideoRepository {
	return &VideoRepository{
		db: db,
	}
}

func (r *VideoRepository) Save(video *domain.Video) (string, error) {
	_, err := r.db.Exec("INSERT INTO videos (id, name) VALUES (?, ?)", video.ID, video.Name)
	if err != nil {
		return "", err
	}
	return video.ID, nil
}

func (r *VideoRepository) FindByID(id string) (*domain.Video, error) {
	row := r.db.QueryRow("SELECT id, name FROM videos WHERE id = ?", id)
	video := &domain.Video{}
	err := row.Scan(&video.ID, &video.Name)
	if err != nil {
		if err == sql.ErrNoRows {
			return nil, nil
		}
		return nil, err
	}
	return video, nil
}

func (r *VideoRepository) FindByName(name string) (*domain.Video, error) {
	row := r.db.QueryRow("SELECT id, name FROM videos WHERE name = ?", name)
	video := &domain.Video{}
	err := row.Scan(&video.ID, &video.Name)
	if err != nil {
		if err == sql.ErrNoRows {
			return nil, nil
		}
		return nil, err
	}
	return video, nil
}

func (r *VideoRepository) FindAll() ([]*domain.Video, error) {
	rows, err := r.db.Query("SELECT id, name FROM videos")
	if err != nil {
		return nil, err
	}
	defer rows.Close()

	var videos []*domain.Video
	for rows.Next() {
		video := &domain.Video{}
		err := rows.Scan(&video.ID, &video.Name)
		if err != nil {
			return nil, err
		}
		videos = append(videos, video)
	}

	return videos, nil
}
