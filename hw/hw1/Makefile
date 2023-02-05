TIMESTAMP := $(shell /bin/date "+%Y-%m-%d-%H:%M:%S")
ZIPNAME := hw1.zip

.PHONY: all oatc test clean zip

all: oatc

oatc: 
	dune build
	@cp bin/main.exe oatc

test: oatc
	./oatc --test

utop:
	utop

zip: bin/simulator.ml
	zip '$(ZIPNAME)' bin/simulator.ml

clean:
	dune clean
	rm -rf oatc ocamlbin bin/main.exe

# 
