.PHONY:	setup-x86_64.exe home

#setup-x86.exe:
#	@echo '##'; echo '## Backup old version'
#	cp -f $@ $@.backup || true
#
#	@echo '##'; echo '## Upgrade to latest $@'
#	wget -N http://www.cygwin.com/$@
#	chmod a+x ./$@
#	cmd.exe /C 'start "Update Cygwin" /MIN cmd.exe /c "$@ && $@"'


setup-x86_64.exe:
	@echo '##'; echo '## Backup old version'
	[ -f $@ ] && cp -f $@ $@.backup

	@echo '##'; echo '## Upgrade to latest $@'
	wget -N https://www.cygwin.com/$@
	chmod a+x ./$@
	# cmd.exe /C 'start "Update Cygwin" /MIN cmd.exe /c "$@ && $@"'
	./$@
	@echo; echo; echo When finished be sure to run "make home"

home:
	[ ! -h /home ] && \
	    rmdir /home && \
	    ln -s /cygdrive/c/Users /home
