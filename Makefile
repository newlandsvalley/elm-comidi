all: examples

.PHONY: examples
examples:
	$(MAKE) -C examples all

clean:
	$(MAKE) -C examples clean

format:
	elm-format src/ examples/

test:
	elm-test
