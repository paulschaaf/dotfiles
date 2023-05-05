ZSH_RESOURCES=zlogin zshrc zshenv
ZSH_RESOURCE_ROOT=~pschaaf/dotfiles/etc/home
OS_SYMLINKS=~/bin/inCygwin ~/bin/inMacOS ~/bin/inLinux

ifeq ($(USER),su)
OTHER_RESOURCES=Makefile bin
endif

LOCAL_ZSH_COPIES=$(patsubst %,$(HOME)/.%,$(ZSH_RESOURCES))
COMPILED_ZSH_RESOURCES=$(patsubst %,%.zwc,$(LOCAL_ZSH_COPIES))

ALL=$(COMPILED_ZSH_RESOURCES) $(OTHER_RESOURCES) ~/bin $(OS_SYMLINKS) .dircolors.zwc
all:	$(ALL)
clean:
	rm -f $(ALL) $(LOCAL_ZSH_COPIES)

Makefile:   ~pschaaf/etc/copyenv.make
	@echo '# If you trust' $^, run the following
	@echo '  'cp -f $^ $@
	@echo '  'make

### OS Symlinks

~/bin/inCygwin:
ifeq ($(UNAME),CYGWIN)
		ln -fs /usr/bin/true $@
else
		ln -fs /usr/bin/false $@
endif

~/bin/inLinux:
ifeq ($(UNAME),Linux)
		ln -fs /usr/bin/true $@
else
		ln -fs /usr/bin/false $@
endif

~/bin/inMacOS:
ifeq ($(UNAME),Darwin)
		ln -fs /usr/bin/true $@
else
		ln -fs /usr/bin/false $@
endif

~/bin:
	mkdir ~/bin

.dircolors.zwc:	~pschaaf/.dircolors.zwc
	cp $^ $@


##### ZSH
.PHONY=zsh
zsh:	$(COMPILED_ZSH_RESOURCES)

$(COMPILED_ZSH_RESOURCES):	%.zwc:	%
	/bin/zsh -c 'zcompile $^'
	@echo

$(LOCAL_ZSH_COPIES):	$(HOME)/.%:	$(ZSH_RESOURCE_ROOT)/%
ifeq ($(USER),pschaaf)
		ln -fs $^ $@
else
		cp -f $^ $@
endif
