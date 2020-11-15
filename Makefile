TARGET_FILES ?= src/Foo/Bar.elm

run: build
	node index.js $(TARGET_FILES)

build: build/Main.js
.PHONY: build

build/Main.js: $(shell find src -iname '*.elm')
	elm make src/Main.elm --output build/Main.js

test:
	WATCHING=false node index.js src/Foo/Bar.elm
	elm-verify-examples && elm-test
