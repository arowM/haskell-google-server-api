.PHONY: build clean ghci haddock-server lint repl run test watch watch-tests watch-test
all: build

clean:
	stack clean

# If you also want to generate haddocks, you might want to do something like this:
#
# stack build --haddock
build:
	stack build --pedantic

# Watch for changes.
watch:
	stack build --file-watch --fast .

# Watch for changes.
watch-test: watch-tests
watch-tests:
	stack test --file-watch --fast .

# Synonym for the ghci target.
repl: ghci

# Run ghci using stack.
ghci:
	stack ghci google-server-api

test:
	stack test

# Run hlint.
lint:
	hlint src/

# This runs a small python websever on port 8000 serving up haddocks for
# packages you have installed.
#
# In order to run this, you need to have run `stack build --haddock`.
haddock-server:
	@(cd .stack-work/install/x86_64-linux/lts-10.4/8.2.2/doc && \
		python -m http.server 8000)
