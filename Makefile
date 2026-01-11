all: run

clean:
	rm -rf dist
	rm -rf elm-stuff

run: build
	cd dist && python3 -m http.server

build: dist/main.js dist/service-worker.js ${subst assets,dist,$(filter-out assets/service-worker.js.in,$(wildcard assets/* assets/icons/*))} 
	echo $^


dist/main.js: src/Main.elm
	elm make $< --output=$@ --optimize

dist/service-worker.js: assets/service-worker.js.in dist/main.js
	python3 generate_service_worker.py

dist/%: assets/%
	cp -r $< $@
