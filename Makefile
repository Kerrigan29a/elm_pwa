all: run

clean:
	rm -rf dist
	rm -rf elm-stuff

run: build
	cd dist && python3 -m http.server

build: dist/main.js ${subst assets,dist,$(wildcard assets/*)}
	python3 list_assets.py
	echo $^

# dist/main.js: src/Main.elm
# 	elm make $< --output=$@ --optimize

dist/main.js: src/Main.elm
	elm make $< --output=$@

dist/%: assets/%
	cp -r $< $@
