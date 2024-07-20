.PHONY: build interact check_format

name:=$(shell basename $(CURDIR))
version:=9.4-slim_0
tag:=$(name):v$(version)

build: docker/Dockerfile Makefile
	docker build \
	  --tag $(tag) \
	  docker

interact: build docker/Dockerfile Makefile
	docker run \
	  --interactive --tty --rm \
	  --volume "$(shell pwd):/remarks" \
	  --workdir "/remarks" \
	  --entrypoint bash \
	  $(tag)

check_format: build Makefile
	docker run \
	  --rm \
	  --volume "$(shell pwd):/remarks" \
	  --workdir "/remarks" \
	  $(tag) \
	  ormolu --mode check $(shell find . -type f -name '*.hs')
