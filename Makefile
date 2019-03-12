################################################################################

slides:  ## Build slides
	mkdir -p dist

	cd the-haskell-epidemic && make slides
	cp -R ./the-haskell-epidemic/dist/ ./dist/the-haskell-epidemic/
	
	cd typeclasses-in-scala && make slides
	cp -R ./typeclasses-in-scala/dist/ ./dist/typeclasses-in-scala/

.PHONY: slides


clean:  ## Clean
	rm -rf dist
	cd the-haskell-epidemic && make clean
	cd typeclasses-in-scala && make clean
.PHONY: clean


################################################################################

help:	## Display this message
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
.PHONY: help
.DEFAULT_GOAL := help
