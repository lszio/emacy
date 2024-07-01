tangle:
	emacs --batch emacy.org --eval "(org-babel-tangle)"
	cd env/python && rye sync && cd ../../
	./bin/epm gen defs

minie: 
	emacs --batch minie.org --eval "(org-babel-tangle)"
