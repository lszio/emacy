.PHONY: envs env-python env-node minie defs vterm-module

envs: env-python env-node

env-python:
	uv sync --project env/python --extra lsp

env-node:
	cd env/node && bun install


minie: 
	emacs --batch minie.org --eval "(org-babel-tangle)"

defs:
	nu bin/epm gen defs

# Compile the vterm native module from the Elpaca source checkout into
# var/modules/ (on `load-path'); vterm.el then loads it without prompting.
vterm-module:
	cd var/elpaca/builds/vterm && mkdir -p build && cd build && cmake .. && make
	mkdir -p var/modules
	cp var/elpaca/builds/vterm/vterm-module.so var/modules/vterm-module.so
