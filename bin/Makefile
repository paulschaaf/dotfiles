# $Source: e:/MyDocuments/cvsroot/bin/Makefile,v $
# $Revision: 1.24 $
# $Date: 2003/11/24 01:35:12 $
# $Author: pschaaf $
# $State: Exp $
# $Name:  $

SSH_LINKS=quick-ssh/d-cc-core quick-ssh/d-cc-merge quick-ssh/d-cc-stable quick-ssh/e-cc-core quick-ssh/e-cc-merge quick-ssh/e-cc-stable

FIND=/usr/bin/find
IGNORE_REGEX='.*\(TAGS\|CVS.*\)'
PRUNE_PATHS=\( -path '*.svn*' -prune \) -o
EXECUTABLE_REGEX='.*\(/\(Makefile\)\|\.\(b?ash\|csh\|dll\|doc\|exe\|js\|ksh\|[Mm]ake\(file\)?\|js\|lisp\|lnk\|rb\|ruby\|sh\|tcsh\|zsh\)\)'
EXECUTABLES=-regex ${EXECUTABLE_REGEX}

NON_TAGGABLE_REGEX='.*\.\(dll\|exe\|jar\|jvi\|lnk\)'
NON_SCRIPT_REGEX='.*\(Makefile\|\.\(java\|txt\)\)'

# Create a new empty file, and rename it to the argument filename. This
# avoids cracker tricks like using a symlink or race condition to redirect
# the overwrite to a different (important) file. See man mktemp.
SAFELY_EMPTY=@mv -f `mktemp $$!.XXXXXX`

#TAGGABLE_REGEX=! -regex '${NON_TAGGABLE_NAMES}\|\.\(${NON_TAGGABLE_EXTNS}\)'
#SCRIPT_REGEX=! -regex '${NON_SCRIPT_NAMES}\|\.\(${NON_SCRIPT_EXTNS}\)'

FIND_DIRS=${FIND}  \( -name .svn -prune \) -o -type d ! -regex ${IGNORE_REGEX}
FIND_FILES=${FIND} \( -name .svn -prune \) -o -type f ! -regex ${IGNORE_REGEX}

FIND_TAGGABLE=${FIND_FILES}   ! -regex ${NON_TAGGABLE_REGEX}
FIND_SCRIPTS=${FIND_TAGGABLE} ! -regex ${NON_SCRIPT_REGEX}

TAGGABLE=$(shell ${FIND_TAGGABLE})
SCRIPTS=$(shell ${FIND_SCRIPTS})

SOFT_LINK=@echo === Symbolically linking $@ to $?; ln -fs $? $@

all:	removeBackups links $(UNAME) TAGS ${HOME}/.profile
	make -C usr-local-bin

ls-exec:
	@${FIND} ${PRUNE_PATHS} -type f ${EXECUTABLES} -print

ls-non-exec:
	@${FIND} ${PRUNE_PATHS} -type f ${EXECUTABLES} -o -print

removeBackups:
	@echo === Removing backups.
	-@cd; ${FIND} . bin -maxdepth 1 \( -regex '.*~$$' -o -name 'nohup.out' \) -exec rm {} \;

fixchmod:
	@echo === Correcting file permissions
	@${FIND} ! -type l -perm +022 -exec chmod og-w {} \; -printf "    WARNING: Non-owner write permission corrected on %P!\n"
	@${FIND_SCRIPTS} ! -perm -755 -exec chmod 755 {} \; -exec ls -la {} \;
#printf "   %m %P"
	-@${FIND_FILES} ! -perm -044 -exec chmod og+r  {} \;
	-@${FIND_DIRS}  ! -perm -055 -exec chmod og+rx {} \;
	@echo

${HOME}/.profile:
	@ln -fs $(shell pwd)/pref_shell $@

ssh_links:	${SSH_LINKS}

${SSH_LINKS}:	ssh-to
	@echo === Symbolically linking $@ to $?; ln -fs ../$? $@
#	${SOFT_LINK}

links:	htmlman cygwin whereizd shortHostName $(shell xrescreen --synonyms) ${SSH-LINKS}

shortHostName:	shortUname
	${SOFT_LINK}

whereizd:	whereiz
	${SOFT_LINK}

ifdef CYGROOT
cygwin:	/bin/true
else
cygwin:	/bin/false
endif

cygwin:
	${SOFT_LINK}

htmlman:	htmlize
	${SOFT_LINK}

$(shell xrescreen --synonyms):	xrescreen
	${SOFT_LINK}

TAGS:	${TAGGABLE}
	@echo === Regenerating $@ file due to changes in:
	@echo '    '$?
	@etags $^

debug:
	@ruby -e 'puts %Q<NON_TAGGABLE_REGEX="$(NON_TAGGABLE_REGEX)"\n\n>'
	@ruby -e 'puts %Q<NON_SCRIPT_REGEX="$(NON_SCRIPT_REGEX)"\n\n>'

	@ruby -e 'puts %Q<FIND_TAGGABLE="$(FIND_TAGGABLE)"\n\n>'
	@ruby -e 'puts %Q<FIND_SCRIPTS="$(FIND_SCRIPTS)"\n\n>'
	@ruby -e 'puts %Q<LN_CYGWIN="$(LN_CYGWIN)"\n\n>'

debugScripts:	debug
	@echo; echo -e TAGGABLE=
	@ruby -e 'puts "$(TAGGABLE)".split(/\.\//).sort.join($$/)' | column | ${PAGER}
	@echo; echo -e SCRIPTS=
	@ruby -e 'puts "$(SCRIPTS)".split(/\.\//).sort.join($$/)' | column | ${PAGER}

CYGWIN:	recode
	-htmlize
	@cd win; make

recode:
#	@echo === Recoding as Unix ASCII.
#	-@recode -v ibmpc..latin1 $(SCRIPTS) 2> /dev/null

Linux:	fixchmod
