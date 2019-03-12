################################################################################

## Since there's a bug in mdx-deck and we can't generate static html, we have to add the Open Graph
## meta tags manually to index.html.
HEAD:= \
<title>Typeclasses in Scala</title>\n\
<meta property='og:title' content='Typeclasses in Scala' />\n\
<meta property='og:image' content='https://talks.diogocastro.com/typeclasses-in-scala/img/card.png' />\n\
<meta property='og:url'   content='https://talks.diogocastro.com/typeclasses-in-scala/' />\n\
<meta name="twitter:creator" content="@dfacastro" />

################################################################################

slides:  ## Build slides
	npm i
	npm run image
	npm run build
	# Remove default <title> tag at line 8
	sed '8d' dist/index.html > dist/temp1
	# Add title/meta tags
	awk -v n=8 -v s="$(HEAD)" 'NR == n {print s} {print}' dist/temp1 > dist/temp2
	cat dist/temp2 > dist/index.html
	rm dist/temp1 dist/temp2

.PHONY: slides


clean:  ## Clean
	rm -rf dist node_modules
.PHONY: clean


################################################################################

help:	## Display this message
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
.PHONY: help
.DEFAULT_GOAL := help


