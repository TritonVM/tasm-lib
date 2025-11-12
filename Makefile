prog :=tasm-lib

debug ?=

$(info debug is $(debug))

# Treat all warnings as errors
export RUSTFLAGS = -Dwarnings

# Set another target dir than default to avoid builds from `make`
# to invalidate cache from barebones use of `cargo` commands.
# The cache is cleared when a new `RUSTFLAGS` value is encountered,
# so to prevent the two builds from interfering, we use two dirs.
export CARGO_TARGET_DIR=./makefile-target

ifdef debug
  release :=
  target :=debug
  extension :=-debug
else
  release :=--release
  target :=release
  extension :=
endif

build:
	$(info RUSTFLAGS is $(RUSTFLAGS))
	cargo build $(release)
	rustup check
	@echo "Update with \`rustup install stable\` if needed."

check:
	cargo check

ctags:
	# Do `cargo install rusty-tags`
	# See https://github.com/dan-t/rusty-tags
	rusty-tags vi

format:
	cargo fmt --all -- --check

clippy:
	cargo clippy --all-targets

happy: format clippy
	RUSTDOCFLAGS="-D warnings" cargo doc --no-deps --workspace --document-private-items
	cargo test --doc

# Get a stack trace upon kernel panic (may slow down implementation)
run: export RUST_BACKTRACE = 1
run:
	$(info RUSTFLAGS is $(RUSTFLAGS))
	cargo run

# Get a stack trace upon kernel panic (may slow down implementation)
test:
	$(info RUSTFLAGS is $(RUSTFLAGS))
	cargo test -- --test-threads=5

bench:
	$(info RUSTFLAGS is $(RUSTFLAGS))
	cargo bench

bench-no-run:
	$(info RUSTFLAGS is $(RUSTFLAGS))
	cargo bench --no-run

all: lint build test bench-no-run

# Run prove/verify on all produced snippets, one at a time. Will take a *long* time. Meant as a test of Triton-VM
# more than a test of the snippets contained in this repo.
prove:
	RUSTFLAGS="-C opt-level=3 -C debug-assertions=no" DYING_TO_PROVE=1 cargo t -- --test-threads=1 --nocapture

help:
	@echo "usage: make [debug=1]"

clean:
	@echo "      ._.  ██    ██  ███  ██ ██ █████    ████ ██    █████  ███  ██  ██"
	@echo "    c/-|   ███  ███ ██ ██ ████  ██      ██    ██    ██    ██ ██ ███ ██"
	@echo "   c/--|   ████████ █████ ███   ███     ██    ██    ███   █████ ██████"
	@echo "   /  /|   ██ ██ ██ ██ ██ ████  ██      ██    ██    ██    ██ ██ ██ ███"
	@echo " mmm ' '   ██    ██ ██ ██ ██ ██ █████    ████ █████ █████ ██ ██ ██  ██"
	@rm -rf target
