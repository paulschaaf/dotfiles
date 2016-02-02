.PHONY:	setup-x86.exe setup-x86_64.exe

setup-x86.exe:
	@echo '##'; echo '## Backup old version'
	cp -f $@ $@.backup || true

	@echo '##'; echo '## Upgrade to latest $@'
	wget -N http://www.cygwin.com/$@
	chmod a+x ./$@
	cmd.exe /C 'start "Update Cygwin" /MIN cmd.exe /c "$@ && $@"'


setup-x86_64.exe:
	@echo '##'; echo '## Backup old version'
	cp -f $@ $@.backup || true

	@echo '##'; echo '## Upgrade to latest $@'
	wget -N http://www.cygwin.com/$@
	chmod a+x ./$@
	cmd.exe /C 'start "Update Cygwin" /MIN cmd.exe /c "$@ && $@"'
