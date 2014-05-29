all:
	@-make -C c
	@-make -C c++
	@-make -C c++11
	@-make -C fpc
	@-make -C go
	@-make -C nimrod

run:
	@-make -C c run
	@-make -C c++ run
	@-make -C c++11 run
	@-make -C fpc run
	@-make -C go run
	@-make -C mruby run
	@-make -C nimrod run
	@-make -C python run

clean:
	@-make -C c clean
	@-make -C c++ clean
	@-make -C c++11 clean
	@-make -C fpc clean
	@-make -C go clean
	@-make -C nimrod clean
