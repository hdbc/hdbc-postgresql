all:
	@echo "Please use Cabal to build this package; not make."
	runghc Setup.lhs configure
	runghc Setup.lhs build

install:
	runghc Setup.lhs install

clean:
	runghc Setup.lhs clean

.PHONY: test
test: test-ghc #test-hugs
	@echo ""
	@echo "All tests pass."

test-hugs:
	@echo " ****** Running hugs tests"
	runghc Setup.lhs configure -f buildtests --hugs
	runghc Setup.lhs build
	runhugs -98 +o -P$(PWD)/dist/scratch:$(PWD)/dist/scratch/programs/runtests: \
		dist/scratch/programs/runtests/Main.hs

test-ghc:
	@echo " ****** Building GHC tests"
	runghc Setup.lhs configure -f buildtests
	runghc Setup.lhs build
	@echo " ****** Running GHC tests"
	./dist/build/runtests/runtests
