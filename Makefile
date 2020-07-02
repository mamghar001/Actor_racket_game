all: build

install:
	raco pkg install raart
	raco pkg install charterm lux

build:
	racket src/main.rkt 2>log.txt
	clear
test:
	racket src/tst.rkt

clean:
	rm -f *[#~]
	rm -f src/*[#~]
	rm -rf src/compiled
