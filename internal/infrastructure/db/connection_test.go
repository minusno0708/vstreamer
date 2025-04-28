package db

import (
	"testing"
)

func TestNewDBConnection(t *testing.T) {
	db, err := NewDBConnection()
	if err != nil {
		t.Fatalf("Failed to connect to database: %v", err)
	}
	defer db.Close()

	if err = db.Ping(); err != nil {
		t.Fatalf("Failed to ping database: %v", err)
	}
}
