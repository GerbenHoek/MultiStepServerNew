.PHONY: default build start clean nuke

image := mbt-server-image

default: build

build: 
	docker build --tag $(image) .

start:
	docker run --rm -it -p 8080:80 -v .:/workspace -w /workspace $(image)

# Docker does not offer a way to ignore non-existing images or containers, so ignore result
clean:
	-docker rmi $(image)

# Remove everything
nuke:
	docker system prune -a
