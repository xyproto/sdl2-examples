.PHONY: all msg clean fullclean run

all: msg main

msg:
	@echo '--- Scala Native ---'

main: Main.scala build.sbt project/plugins.sbt
	sbt nativeLink
	@mv -f ./target/scala-*/scalanative-out ./main

run: msg main
	@#time sbt run
	time ./main

clean:
	@#sbt clean
	@-rm -f main

fullclean: clean
	@-rm -f project/build.properties
	@-rm -rf target project/{project,target}
