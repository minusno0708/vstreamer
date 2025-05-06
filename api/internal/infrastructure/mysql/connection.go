package mysql

import (
	"database/sql"

	_ "github.com/go-sql-driver/mysql"
)

func NewConnection() (*sql.DB, error) {
	db, err := sql.Open("mysql", "root:root@tcp(db:3306)/contents_db")
	if err != nil {
		return nil, err
	}
	if err = db.Ping(); err != nil {
		return nil, err
	}

	return db, nil
}
