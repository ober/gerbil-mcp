.PHONY: all install build clean update

all: install build

install:
	npm install

build:
	npm run build

clean:
	rm -rf dist node_modules

update:
	mkdir -p ~/.claude
	cp CLAUDE.md.gerbil-example ~/.claude/CLAUDE.md
