.PHONY: msg run clean

all: msg main

msg:
	@echo '--- Dart ---'

pubspec.lock: pubspec.yaml
	dart pub get

main: pubspec.lock main.dart
	dart compile exe main.dart -o main

run:
	time ./main

clean:
	rm -f main

fullclean: clean
	rm -f pubspec.lock
