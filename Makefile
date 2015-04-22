SAMPLES = c c++ c++11 fpc go mruby nimrod python rust lua

all:
	@-$(foreach x,$(SAMPLES),make -C $(x);)

run:
	@-$(foreach x,$(SAMPLES),time make -C $(x) run;)

clean:
	@-$(foreach x,$(SAMPLES),make -C $(x) clean;)
