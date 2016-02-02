# $Source: e:/MyDocuments/cvsroot/etc/Makefile,v $
# $Revision: 1.49 $
# $Date: 2004/05/11 23:12:44 $
# $Author: pschaaf $
# $State: Exp $
# $Name:  $

MAKEABLE_SUBDIRS=$(strip $(shell find -mindepth 2 -maxdepth 2 -path '*~*' -prune -o -type f -name 'Makefile' -printf '%h '))

UNAME=$(subst _NT-5.0,,$(shell uname))

.PHONY:	${MAKEABLE_SUBDIRS} ${UNAME} all chmod clean cleanLinks cleanSubdirs debug links local missing rebuild sources

RUBYLIB=$(HOME)/lib/ruby:$(HOME)/bin

TO_PATH=tr ' ' ':' | sed 's!\(^\|:\)${HOME}!\1~!g'
TO_PATH_THEN_ECHO_AS_LIST=${TO_PATH} | tr ':' '\n' | cat -n

HOME_LINKS=XWinrc Xresources acoc.conf dtprofile enscriptrc irbrc ispell_english lftp lynxrc ncftp netrc plan pluckerrc ssh surfraw.conf toprc vimrc.more xemacs xinitrc work.sig

COMPUTE_SRC_NAME=sed 's:^${HOME}/\.\{0,1\}::; s:/.*/\(${LOGNAME}[-_.]\)\{0,1\}:/:; s:^:${PWD}/:'

all:	local ${MAKEABLE_SUBDIRS} sources

ifdef CYGROOT
# RECODE_FILES=`file * | ruby -ne 'print split(":").shift, " " if \$$_ =~ /ASCII.*CR/'`

# ${UNAME}:
# 	@echo === Recoding as Unix ASCII.
# 	@for file in $(RECODE_FILES); do recode -v ibmpc..latin1 $$file; done
# 	htmlize
# else

# ${UNAME}:	chmod
endif

LINKS=$(addprefix ${HOME}/.,${HOME_LINKS}) ${UNAME_TARGETS}

SOURCES=$(subst ${PWD},.,$(foreach x,${LINKS},$(shell echo ${x} | ${COMPUTE_SRC_NAME})))
sources:	${SOURCES}

local:	links ${UNAME_TARGETS} ${UNAME}

links:	${LINKS}

missing:
	@$(foreach x,${LINKS},s=$(subst ${PWD},.,$(shell echo ${x} | ${COMPUTE_SRC_NAME})); [ ! -e $$s ] && mv ${x} $$s;)

${MAKEABLE_SUBDIRS}:
	@echo; echo \#\# Making $@
	@-cd $@; make

clean:	cleanLinks cleanSubdirs

cleanSubdirs:
	@echo
	@for x in ${MAKEABLE_SUBDIRS}; \
		do echo \#\# Cleaning $$x; \
		make -C $$x clean; \
		echo; \
	done

cleanLinks:
	@echo
	rm -f ${LINKS}

rebuild:	clean all

chmod:
	-@find -path './CVS' -prune -o -maxdepth 1 \( -type f ! -perm 644 -exec chmod 644 {} \; -o -type d ! -perm 755 -exec chmod 755 {} \; \) -printf "Fixed permissions of %p\n"
#	-@find -maxdepth 1 -name 'crontab' ! -perm 600 -exec chmod 600 {} \; -printf "Fixed permissions of %p\n"
	-@find cron.*/ -maxdepth 1 -type f ! -perm 755 -exec chmod 755 {} \; -printf "Fixed permissions of %p\n"
#	-@find cron.* -path './CVS' -prune -o -path './cron.*' ! -perm 700 -exec chmod 700 {} \; -printf "Fixed permissions of %p\n"

%:
	-ln -s `echo $(@F) | sed 's:^\.\{0,1\}:etc/:'` $@

debug:
	@echo \$$UNAME=${UNAME}; echo
	@echo; echo \$$LINKS:
	@echo ${LINKS}   | ${TO_PATH_THEN_ECHO_AS_LIST}

	@echo; echo \$$SOURCES:
	@echo ${SOURCES} | ${TO_PATH_THEN_ECHO_AS_LIST}
	@echo
	@echo \$$MAKEABLE_SUBDIRS=${MAKEABLE_SUBDIRS}; echo
	@echo 'Add prefix $${HOME} to empty var ->'  $(addprefix ${HOME}/,${FOO})

	@echo; echo \$$UNAME_TARGETS:
	@echo ${UNAME_TARGETS}
