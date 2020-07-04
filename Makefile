EMACS?=emacs

.PHONY: clean setup all

init.el: init.org
	$(EMACS) --batch -l org --eval "(org-babel-tangle-file \"./init.org\")"

setup: init.el

clean:
	rm -f init.el
