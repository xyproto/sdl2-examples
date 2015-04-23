.PHONY: all msg clean fullclean

all: msg go

msg:
	@echo --- Go ---

go: deps main.go
	go build

deps:
	go get -d

run: msg go
	./go

gorun: main.go
	go run main.go

clean:
	go clean

fullclean: clean
