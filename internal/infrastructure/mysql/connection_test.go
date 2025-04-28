package mysql

import (
	"testing"
)

func TestNewConnection(t *testing.T) {
	db, err := NewConnection()
	if err != nil {
		t.Fatalf("Failed to connect to database: %v", err)
	}
	defer db.Close()

	if err = db.Ping(); err != nil {
		t.Fatalf("Failed to ping database: %v", err)
	}
}
