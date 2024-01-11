EMACS?=emacs

.PHONY: clean setup all

init.el: init.org
	$(EMACS) --batch -l org --eval "(org-babel-tangle-file \"./init.org\")"

node_modules/typescript-eslint-language-service:
	npm i typescript-eslint-language-service

setup: init.el node_modules/typescript-eslint-language-service

clean:
	rm -f init.el
