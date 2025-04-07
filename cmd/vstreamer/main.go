package main

import (
	"fmt"
	"log"
	"net/http"
	"strings"
)

func main() {
	http.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		fmt.Fprintf(w, "Hello, world!")
	})

	http.HandleFunc("/pages/", func(w http.ResponseWriter, r *http.Request) {
		pageName := strings.TrimPrefix(r.URL.Path, "/page/")
		fmt.Fprintf(w, "show %s page", pageName)
	})

	http.HandleFunc("/streams/", func(w http.ResponseWriter, r *http.Request) {
		videoPath := strings.TrimPrefix(r.URL.Path, "/stream/")
		fmt.Fprintf(w, "stream %s video", videoPath)
	})

	http.HandleFunc("/videos/", func(w http.ResponseWriter, r *http.Request) {
		videoId := strings.TrimPrefix(r.URL.Path, "/videos/")
		fmt.Fprintf(w, "show %s video", videoId)
	})

	http.HandleFunc("/upload", func(w http.ResponseWriter, r *http.Request) {
		fmt.Fprintf(w, "upload video")
	})

	log.Fatal(http.ListenAndServe(":8080", nil))
}
