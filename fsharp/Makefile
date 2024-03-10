.PHONY: all msg clean

all: msg bin

msg:
	@echo '--- F# ---'

bin: main.fsproj main.fs
	dotnet build --sc --nologo --ucr

run: msg bin
	time dotnet run

clean:
	rm -rf bin/ obj/
