package mysql

import (
	"testing"
)

func TestNewConnection(t *testing.T) {
	db, err := NewConnection("root:root@tcp(localhost:3306)/contents_db")
	if err != nil {
		t.Fatalf("Failed to connect to database: %v", err)
	}
	defer db.Close()

	if err = db.Ping(); err != nil {
		t.Fatalf("Failed to ping database: %v", err)
	}
}
