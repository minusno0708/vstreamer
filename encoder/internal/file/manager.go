package file

import "os"

func IsExist(path string) bool {
	_, err := os.Stat(path)
	if os.IsNotExist(err) {
		return false
	}
	return true
}

func RemoveFile(path string) error {
	err := os.Remove(path)
	if err != nil {
		return err
	}
	return nil
}

func CreateDir(path string) error {
	err := os.MkdirAll(path, os.ModePerm)
	if err != nil {
		return err
	}
	return nil
}

func RemoveDir(path string) error {
	err := os.RemoveAll(path)
	if err != nil {
		return err
	}
	return nil
}
