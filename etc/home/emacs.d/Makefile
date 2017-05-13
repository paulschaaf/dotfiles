.PHONY:	chmod links
.SUFFIXES:	.el

MODE=0644
UMASK=0133

LINKS=${HOME}/.emacs ${HOME}/.xemacs site-lisp
ELISP_FILENAMES=$(shell ls *.el)
COMPILED_ELISP_FILENAMES=$(addsuffix c,${ELISP_FILENAMES})

CHMOD=$(shell find ${ELISP_FILENAMES} -type f ! -perm ${MODE})

%.elc:	%.el
	@umask ${UMASK}; emacs -batch -f batch-byte-compile $?

all:	$(if ${CHMOD},chmod) ${LINKS} ${COMPILED_ELISP_FILENAMES}

clean:
	rm -f *~ ${COMPILED_ELISP_FILENAMES}

chmod:
	@echo Changing permissions of ${CHMOD}
	@chmod ${MODE} ${CHMOD}

debug:
	@echo ELC_FILES=${ELC_FILES}

rebuild:	clean all

links:	${LINKS}

${HOME}/.emacs:	$(shell pwd)/custom.el
	-ln -fs $? $@

${HOME}/.xemacs:	$(shell pwd)
	-ln -fs $(subst $(dir $@),,$?) $@

site-lisp:
	-ln -s ${HOME}/local/sync/software/emacs $@
