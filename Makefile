run:
	docker compose up -d
restart:
	docker compose restart
reset:
	docker compose down --volumes
	sudo rm -rf ./contents
	mkdir ./contents
