.PHONY: all msg clean fullclean

all: msg go

msg:
	@echo '--- Go ---'

go: main.go
	go build -mod=vendor -v

run: msg go
	time ./go

gorun: main.go
	go run main.go

clean:
	go clean

fullclean: clean
