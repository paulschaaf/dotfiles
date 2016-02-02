# $Source: e:/MyDocuments/cvsroot/etc/makefiles/template.make,v $
# $Revision: 1.10 $
# $Date: 2004/02/14 02:41:28 $
# $Author: pschaaf $
# $State: Exp $
# $Name:  $

MAKEABLE_SUBDIRS=$(strip $(shell find -mindepth 2 -maxdepth 2 -type f -name 'Makefile' -printf '%h '))

RUBYLIB_DEFAULT=$$HOME/lib/ruby:$$HOME/bin
RUBY=RUBYLIB=$${RUBYLIB:-${RUBYLIB_DEFAULT}} ruby

.PHONY:	prereq links chmod ${MAKEABLE_SUBDIRS}

UNAME=$(subst _NT-5.0,,$(shell uname))
RUBYLIB=$(shell echo $${RUBYLIB:-$$HOME/lib/ruby:$$HOME/bin})

FILES_NEEDING_RECODE=`file * | ruby -ne 'print split(":").shift, " " if \$$_ =~ /ASCII.*CR/'`

HARD_LINK=echo === Linking $@ to $?; ln -f  $(shell pwd)/$? $@
SOFT_LINK=echo === Symbolically linking $@ to $?; ln -fs $(shell pwd)/$? $@

# ================================================
# ======= SUPERUSER

ifeq (0,$(shell id -u))
SUPERUSER=true
endif

# ================================================
# ======= Safe File Updates

# Create a new empty file, and rename it to the argument filename. This
# avoids cracker tricks like using a symlink or race condition to redirect
# the overwrite to a different (important) file. See man mktemp.

TEMP_VAR=temp_f
TEMP=$$${TEMP_VAR}
MKTEMP=${TEMP_VAR}=`mktemp $$TMP/$$$$.XXXXXX`
MKTEMPDIR=${TEMP_VAR}=`mktemp -d $$TMP/$$$$.XXXXXX`

MOVE_TEMP_TO=mv -f ${TEMP}
MOVE_TEMPD_FILES_TO=${MOVE_TEMP_TO}/*

THROUGH_TEMP_INTO=${TEMP}; ${MOVE_TEMP_TO}
THROUGH_TEMPD_INTO=${TEMP}; ${MOVE_TEMPD_FILES_TO}

#safely_copy_into () {
#	local dest temp_dir
#	dest=$1; shift
#	temp_dir=`mktemp -d $$TMP/$$$$.XXXXXX`
#	cp $* $temp_dir
#	mv -f $temp_dir/* $1
#}

# Examples:
#	${MKTEMP}; cat source >| ${THROUGH_TEMP_TO} dest
#	${MKTEMP}; cp  source    ${THROUGH_TEMP_TO} dest

#	${MKTEMPD}; cp f1 f2 ... ${THROUGH_TEMPD_TO} dest

# ================================================
# ================================================

all:	${MAKEABLE_SUBDIRS} ${UNAME}

subdirs: ${MAKEABLE_SUBDIRS}

${MAKEABLE_SUBDIRS}:
	@echo; echo \#\# Making $@
	@cd $@; make

clean:	cleanMakeableSubdirs
	@echo
	@echo rm -f ${LINKS}

cleanMakeableSubdirs:
	@echo
	@for x in ${MAKEABLE_SUBDIRS}; \
		do echo \#\# Cleaning $$x; \
		make -C $$x clean; \
		echo; \
	done

rebuild:	clean all

chmod:
	@echo Fixing file permissions.
	@-find -maxdepth 1 -type f               | xargs chmod 644
	@-find -maxdepth 1 -type d ! -name 'CVS' | xargs chmod 755

CYGWIN:	CYGWIN_links recode

recode:
	@echo === Recoding as Unix ASCII.
	-@for file in $(RECODE_FILES); do recode -v ibmpc..latin1 $$file; done

Linux:	Linux_links chmod

CYGWIN_links:

Linux_links:	${HOME}/.kde/share/apps/kdisplay/color-schemes/$(shell whoami).kcsrc

debug:
	@ruby -e 'puts %Q<MAKEABLE_SUBDIRS="$(MAKEABLE_SUBDIRS)"\n\n>'
