.PHONY:	config local

all:	config local

config local:
	@echo "\n==== Making links to files in '$@'"
	find $@ -type f -exec ln -s {} ~/.{} \;
