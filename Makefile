tangle:
	emacs --batch emacy.org --eval "(org-babel-tangle)"
	./bin/epm gen defs

minie: 
	emacs --batch minie.org --eval "(org-babel-tangle)"