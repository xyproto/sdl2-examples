.PHONY: all msg clean fullclean

all: msg main

msg:
	@echo '--- C# ---'

main.exe: main.cs
	mcs -unsafe -out:$@ $<

main: main.exe
	mkbundle -o $@ --deps --simple --static --i18n none --machine-config /etc/mono/4.5/machine.config --config /etc/mono/config $<
	rm -f main.exe

run: msg main
	time ./main

clean:
	rm -f main main.exe temp.* *crash*.json
