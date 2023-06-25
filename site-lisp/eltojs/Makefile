.PHONY: test

clean:
	rm -rf build

build/eltojs-e2e-test.mjs: eltojs-e2e-test.el
	mkdir build
	emacs --batch -l ./eltojs.el -l ./eltojs-lib.el --eval "(eltojs-compile-file \"$<\" \"$@\")"

test: clean build/eltojs-e2e-test.mjs
	node --test build/*.mjs
