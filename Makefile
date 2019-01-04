SAMPLES = c11 c++98 c++11 d fpc go mruby nim pony python lua

# These two currently does not build:
# SAMPLES += ocaml rust

# gccgo works, but installing gccgo conflicts with go
# SAMPLES += gccgo

.PHONY: all run clean fullclean

all:
	@-$(foreach x,$(SAMPLES),make -C $(x);)

run:
	@-$(foreach x,$(SAMPLES),make -C $(x) run;)

clean:
	@-$(foreach x,$(SAMPLES),make -C $(x) clean;)

fullclean:
	@-$(foreach x,$(SAMPLES),make -C $(x) fullclean;)
