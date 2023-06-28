.PHONY: all msg clean fullclean check-config

MONO_VERSION ?= 4.5

ifeq ($(shell uname -s), Darwin)
	MONO_ETC := $(shell dirname $(shell dirname $(shell realpath $(shell which mono))))/etc/mono
else
	MONO_ETC := /etc/mono
endif

MONO_CONFIG := $(MONO_ETC)/config
MACHINE_CONFIG := $(MONO_ETC)/$(MONO_VERSION)/machine.config

all: msg check-config main

msg:
	@echo '--- C# ---'

main.exe: main.cs
	mcs -unsafe -out:$@ $<

main: main.exe
	@echo "Mono config file: $(MONO_CONFIG)"
	@echo "Machine config file: $(MACHINE_CONFIG)"
	mkbundle -o $@ --deps --simple --static --i18n none --machine-config $(MACHINE_CONFIG) --config $(MONO_CONFIG) $<
	rm -f main.exe

run: msg main
	time ./main

clean:
	rm -f main main.exe temp.* *crash*.json

check-config:
	@if [ ! -f $(MACHINE_CONFIG) ]; then \
		echo "ERROR: The file $(MACHINE_CONFIG) does not exist."; \
		exit 1; \
	fi
	@if [ ! -f $(MONO_CONFIG) ]; then \
		echo "ERROR: The file $(MONO_CONFIG) does not exist."; \
		exit 1; \
	fi
