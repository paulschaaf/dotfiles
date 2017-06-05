.PHONY:	config local

all:	config local

config local:
	@echo "\n==== Making links to files in '$@'"
	find $@ -type f -exec ln -fs ${PWD}/{} ~/.{} \;

dolphin:
	dolphin --split config ~/.config &
	dolphin --split local  ~/.local  &
