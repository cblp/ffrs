# build

build: rs hs

.PHONY: rs
rs:
	cd rs && cargo build

.PHONY: hs
hs: rs
	cd hs && stack build

# test

.PHONY: test
test: rs/test hs/test

.PHONY: rs/test
rs/test:
	cd rs && cargo test

.PHONY: hs/test
hs/test: rs
	cd hs && stack test
