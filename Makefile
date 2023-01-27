.PHONY: build interact

name:=$(shell basename $(CURDIR))
version:=9.4-slim_0
tag:=$(name):v$(version)

build: docker/Dockerfile Makefile
	docker build \
	  --tag $(tag) \
	  docker

interact: build docker/Dockerfile Makefile
	mkdir -p .home
	docker run \
	  --interactive --tty --rm \
	  --volume "$(shell pwd):/src" \
	  --volume "$(shell pwd)/.home:/home/alis" \
	  --workdir "/src" \
	  --entrypoint bash \
	  $(tag)
