# Based on https://github.com/roman/Haskell-capataz/blob/master/Makefile

################################################################################

STACK:=stack $(STACK_ARGS)

################################################################################

ghcid:  ## Launch ghcid
	ghcid \
		--command "stack ghci" \
		--restart package.yaml
.PHONY: ghcid

ghcid-test:  ## Launch ghcid and automatically run tests
	ghcid \
		--command "stack ghci" \
		--test main \
		--restart package.yaml
.PHONY: ghcid-unit

################################################################################

help:	## Display this message
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
.PHONY: help
.DEFAULT_GOAL := help
