.PHONY: all msg clean fullclean

all: msg go

msg:
	@echo --- Go ---

go: deps main.go
	go build -v

deps:
	go get -d

run: msg go
	time ./go

gorun: main.go
	go run main.go

clean:
	go clean

fullclean: clean
