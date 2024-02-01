PLATFORMS = linux-i386 linux-x86_64

.PHONY: all run clean fullclean

all:
	@-$(foreach x,$(PLATFORMS),make -C $(x);)

run:
	@-$(foreach x,$(PLATFORMS),make -C $(x) run;)

clean:
	@-$(foreach x,$(PLATFORMS),make -C $(x) clean;)

fullclean:
	@-$(foreach x,$(PLATFORMS),make -C $(x) fullclean;)
