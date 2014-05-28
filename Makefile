all:
	@-make -C c
	@-make -C c++
	@-make -C c++11
	@-make -C fpc
	@-make -C go
	#@-make -C mruby (interpreted)
	@-make -C nimrod

run:
	@-make -C c run
	@-make -C c++ run
	@-make -C c++11 run
	#@-make -C fpc run (fails to run)
	@-make -C go run
	@-make -C mruby run
	@-make -C nimrod run

clean:
	@-make -C c clean
	@-make -C c++ clean
	@-make -C c++11 clean
	@-make -C fpc clean
	@-make -C go clean
	#@-make -C mruby clean (no cleaning needed)
	@-make -C nimrod clean
