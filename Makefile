SAMPLES = c89 c99 c11 c18 c++98 c++11 d objectivepascal go haskell mruby nim odin pony python ring rust v zig

# These two currently does not build:
# SAMPLES += ocaml

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
