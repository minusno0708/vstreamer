run:
	docker compose up -d
restart:
	docker compose restart
reset:
	docker compose down
	find ./videos -mindepth 1 -maxdepth 1 -type d ! -name '.gitignore' -exec rm -rf {} +

