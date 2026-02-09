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
	cp copilot-instructions.md.gerbil-example ~/.copilot-instructions.md
	mkdir -p ~/.claude/skills/save-discoveries
	cp .claude/skills/save-discoveries/SKILL.md ~/.claude/skills/save-discoveries/SKILL.md
