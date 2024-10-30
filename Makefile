compile:
	rebar3 compile

run:
	docker compose up -d
	rebar3 shell

lint:
	rebar3 lint
