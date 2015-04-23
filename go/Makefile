.PHONY: all clean fullclean

all: go

go: deps main.go
	go build

deps:
	go get -d

run: go
	@echo --- Go ---
	./go

gorun: main.go
	@echo --- Go run ---
	go run main.go

clean:
	go clean

fullclean: clean
