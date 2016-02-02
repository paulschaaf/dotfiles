.SUFFIXES:
USER=$(shell whoami)
SOURCE_USER=pschaaf
SOURCE_HOST=${HOST}
SOURCE_ROOT=~${SOURCE_USER}/
MAKEFILE_ROOT=$(SOURCE_ROOT)etc/makefiles
ENVFILE_ROOT=$(SOURCE_ROOT)etc/env
ENVFILE_PREFIX=$(ENVFILE_ROOT)/$(shell hostname -s)

# if the destination is missing create a symbolic link to the source, 
# otherwise copy it

DIRS=.ssh .xemacs
EMACS_FILES=.xemacs/abbreviations .xemacs/custom.el .xemacs/init.el .xemacs/rotate-buffers.el
SSH_FILES=.ssh/config .ssh/known_hosts
RC_FILES=.bashrc .screenrc .tcshrc .zshrc
OTHER_FILES=.less .toprc
SOFT_LINKS=.emacs

# root should only copy files
# everyone else will only copy if files already exist
ifeq (0,$(shell id -u))
REFRESH=sudo -u pschaaf cat $< >| $@
#REFRESH=cp -f $< $@
else
REFRESH=if [ -f $@ ] && [ ! -h $@ ]; \
	then echo Copying $< to $@ && cp -f $< $@; \
	else echo Creating link from $< to $@ && ln -fs $< $@; \
fi
USER_FILES=
#echo Linking $@ to $< && ln -fs $< $@
endif

######################### GENERIC
ALL_EXCEPT_MAKEFILE=$(EMACS_FILES) ${RC_FILES} $(OTHER_FILES) $(USER_FILES)

all clean:	Makefile
	-@make $@_except_$<

all_except_Makefile:	${DIRS} ${ALL_EXCEPT_MAKEFILE} ${SOFT_LINKS}

clean_except_Makefile:
	rm -rf ${SOFT_LINKS} ${ALL_EXCEPT_MAKEFILE} ${DIRS}

rebuild:	clean;	@echo; echo Rebuilding.; make

debug:
	@echo id = `id -u`
	@if [ `id -u` -eq 0 ]; then echo I\'m root.; else echo I\'m not root; fi

Makefile:	$(MAKEFILE_ROOT)/newuser.make
	@$(REFRESH)
	@echo Retrying with refreshed $@.; echo

${ALL_EXCEPT_MAKEFILE}:; -@$(REFRESH)

${DIRS}:; mkdir $@

${SOFT_LINKS}:; ln -fs $< $@

######################### shell
#.bashrc:	$(ENVFILE_PREFIX).bash
.screenrc:	$(ENVFILE_PREFIX).screen
.tcshrc:	$(ENVFILE_PREFIX).tcsh
.zshrc:	$(ENVFILE_PREFIX).zsh

######################### ssh
#.ssh/authorized_keys:	${SOURCE_ROOT}.ssh/authorized_keys
.ssh/config:	${SOURCE_ROOT}.ssh/config
.ssh/known_hosts:	${SOURCE_ROOT}.ssh/known_hosts

######################### xemacs
.xemacs/abbreviations:	$(SOURCE_ROOT).xemacs/abbreviations
.xemacs/custom.el:	$(SOURCE_ROOT).xemacs/custom.el
.xemacs/init.el:	$(SOURCE_ROOT).xemacs/init.el
.xemacs/rotate-buffers.el:	$(SOURCE_ROOT).xemacs/rotate-buffers.el

######################### other
.less:	$(SOURCE_ROOT).less_${HOSTNAME}
.toprc:	$(SOURCE_ROOT).toprc

######################### soft links
.emacs:	.xemacs/init.el
