run:
	docker compose up -d
restart:
	docker compose restart
reset:
	docker compose down
	sudo rm -rf ./videos
