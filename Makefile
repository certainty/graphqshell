demo:
	docker-compose -f docker/docker-compose.yml run --rm tui

build_image:
	docker-compose -f docker/docker-compose.yml build  
