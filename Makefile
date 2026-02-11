.PHONY: all install build test clean update

all: install build

install:
	npm install

build:
	npm run build

test: build
	npm run test

clean:
	rm -rf dist node_modules

update:
	mkdir -p ~/.claude
	cp CLAUDE.md.gerbil-example ~/.claude/CLAUDE.md
	cp copilot-instructions.md.gerbil-example ~/.copilot-instructions.md
	mkdir -p ~/.claude/skills/save-discoveries
	cp .claude/skills/save-discoveries/SKILL.md ~/.claude/skills/save-discoveries/SKILL.md
	@if command -v gemini > /dev/null; then \
		echo "Updating Gemini MCP configuration..."; \
		gemini mcp add gerbil node $(shell pwd)/dist/index.js -e GERBIL_MCP_GXI_PATH=$(shell which gxi || echo /opt/gerbil/bin/gxi) --scope user --trust; \
	fi
