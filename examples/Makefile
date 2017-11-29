all: exampleParser

exampleParser: distjs/exampleParser.js
distjs/exampleParser.js: parser/Parser.elm
	elm-make parser/Parser.elm --output=distjs/exampleParser.js

clean:
	rm distjs/*.js

format:
	elm-format parser/ performer/
