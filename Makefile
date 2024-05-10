all: run

clean:
	rm -rf dist

run: build
	cd dist && python3 -m http.server

build: dist/main.js ${subst assets,dist,$(wildcard assets/*)}
	echo $^

dist/main.js: src/Main.elm
	elm make $< --output=$@ --optimize

dist/%: assets/%
	cp $< $@
