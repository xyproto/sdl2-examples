.PHONY: all msg clean fullclean

all: msg main

msg:
	@echo '--- Rust ---'

main:
	cargo build --release

run: msg main
	time ./target/release/rust

clean:
	cargo clean

fullclean: clean
	rm -f Cargo.lock
