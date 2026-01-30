.PHONY: all install build clean

all: install build

install:
	npm install

build:
	npm run build

clean:
	rm -rf dist node_modules
