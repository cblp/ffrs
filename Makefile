all: core cli

test: core/test cli/test

.PHONY: core
core:
	cd core && cargo build

.PHONY: cli
cli: core
	cd cli && stack build

.PHONY: core/test
core/test:
	cd core && cargo test

.PHONY: cli/test
cli/test: core
	cd cli && stack test
