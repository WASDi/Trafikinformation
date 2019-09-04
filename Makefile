TMPFILE := $(shell mktemp)

all:
	stack build

run:
	stack exec trafikinformation-exe

ghci:
	stack exec ghci -- -isrc src/Trafikinformation.hs

ghcid:
	ghcid -c 'stack ghci trafikinformation'

ghcid-test:
	ghcid -c 'stack ghci test/Spec.hs'

dependencies:
	stack dot --external > $(TMPFILE)
	xdot $(TMPFILE)
	rm $(TMPFILE)
