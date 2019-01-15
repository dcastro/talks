################################################################################

## Since there's a bug in mdx-deck and we can't generate static html, we have to add the Open Graph
## meta tags manually to index.html.
HASKELL_EPIDEMIC_META:= \
<title>The Haskell Epidemic</title>\n\
<meta property='og:title' content='The Haskell Epidemic' />\n\
<meta property='og:image' content='https://talks.diogocastro.com/the-haskell-epidemic/img/card.png' />\n\
<meta property='og:url'   content='https://talks.diogocastro.com/the-haskell-epidemic/' />\n\
<meta name="twitter:creator" content="@dfacastro" />

################################################################################

slides:  ## Build slides
	mkdir -p dist && \
	cd the-haskell-epidemic && \
	npm i && \
	npm run image && \
	cp dist/card.png img && \
	npm run build && \
	sed '8d' dist/index.html > dist/temp1 && \
	awk -v n=8 -v s="$(HASKELL_EPIDEMIC_META)" 'NR == n {print s} {print}' dist/temp1 > dist/temp2 && \
	cat dist/temp2 > dist/index.html && \
	rm dist/temp1 dist/temp2 && \
	cp -R dist/ ../dist/the-haskell-epidemic/
.PHONY: slides


clean:  ## Clean
	rm -rf dist the-haskell-epidemic/dist the-haskell-epidemic/node_modules
.PHONY: clean


################################################################################

help:	## Display this message
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
.PHONY: help
.DEFAULT_GOAL := help
