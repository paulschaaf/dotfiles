all:	chmod

chmod:
	@echo Fixing file permissions.
	@-find -maxdepth 1 -type f               | xargs chmod 644
	@-find -maxdepth 1 -type d ! -name 'CVS' | xargs chmod 755
