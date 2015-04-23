SAMPLES = c c++ c++11 fpc go mruby nim python lua ocaml

.PHONY: all run clean fullclean

all:
	@-$(foreach x,$(SAMPLES),make -C $(x);)

run:
	@-$(foreach x,$(SAMPLES),time make -C $(x) run;)

clean:
	@-$(foreach x,$(SAMPLES),make -C $(x) clean;)

fullclean:
	@-$(foreach x,$(SAMPLES),make -C $(x) fullclean;)
