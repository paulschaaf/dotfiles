.SUFFIXES:

SRC_SOURCE=$(shell pwd)
SRC_FILES=$(shell find -maxdepth 1 -type f ! \( -name '*~' -o -name 'Makefile' -o -name '.htmlize_targets' -o -name '.installed' -o -name '*.lnk' -o -name '*.old' \) -printf '%f ')
SYNONYMABLE_FILES=d2u launch.sh mozclient

DEST=/usr/local/bin/

OWNER=pschaaf
PERM=$(if ${CYGROOT},700,755)

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
RMTEMPDIR=rm -rf ${TEMP}

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

SRC_SYNONYM_FILES=$(shell ./makesynonyms -l $(addprefix ${SRC_SOURCE}/,${SYNONYMABLE_FILES}))
DEST_FILES=$(addprefix ${DEST},${SRC_FILES})
DEST_SYNONYMABLE_FILES=$(addprefix ${DEST},${SYNONYMABLE_FILES})
DEST_SYNONYM_FILES=$(addprefix ${DEST},${SRC_SYNONYM_FILES})

MY_FILES=$(if ${SUPERUSER},.installed,${SRC_FILES})

# ================================================

.PHONY:	all

ifdef SUPERUSER
all:	$(if ${DEST_SYNONYM_FILES},${DEST_SYNONYM_FILES},rerun_make) ${MY_FILES} chown chmod
else
all:	${MY_FILES} chmod
	$(if ${CYGROOT},@SUPERUSER=true make)
endif

chmod:
	@find ${MY_FILES} $(if ${SUPERUSER},,Makefile) ! -name '.installed' ! -perm ${PERM}  -print | xargs -r -t chmod ${PERM} 2>&1

chown:
	@find ${MY_FILES} ${DEST_FILES} ! -user ${OWNER} ! -name '.installed' -print | xargs -r -t chown ${OWNER}

.installed:	${SRC_FILES}
	${MKTEMPDIR}; cp --no-dereference --preserve $^ ${THROUGH_TEMPD_INTO} ${DEST}; ${RMTEMPDIR}
	touch $@

rerun_make:
	make

rebuild:	clean all

clean:
	-rm -f *~ ${SRC_SYNONYM_FILES} $(if ${SUPERUSER}${CYGROOT},${DEST_FILES} ${DEST_SYNONYM_FILES})

${DEST_FILES}:	${SRC_FILES}
ifdef CYGROOT
	-ln -s $(addprefix $(shell pwd)/,$?) ${DEST}
else
	cp $? ${DEST}
endif

$(addprefix ${DEST},$(shell ./launch.sh --synonyms)):	${DEST}launch.sh
	cd ${DEST}; ln -fs $(notdir $?) $@ $(if ${SUPERUSER},,2> /dev/null || true)

$(addprefix ${DEST},$(shell ./mozclient --synonyms)):	${DEST}mozclient
	cd ${DEST}; ln -fs $(notdir $?) $@ $(if ${SUPERUSER},,2> /dev/null || true)

$(addprefix ${DEST},$(shell ./d2u --synonyms)):	${DEST}d2u
	cd ${DEST}; ln -fs $(notdir $?) $@ $(if ${SUPERUSER},,2> /dev/null || true)

debug:
	@echo MY_FILES=${MY_FILES}
	@echo
	@echo SRC_FILES=${SRC_FILES}
	@echo
	@echo SRC_SYNONYM_FILES=${SRC_SYNONYM_FILES}
	@echo
	@echo SYNONYMABLE_FILES=${SYNONYMABLE_FILES}
	@echo
	@echo DEST_FILES=${DEST_FILES}
	@echo
	@echo DEST_SYNONYMABLE_FILES=${DEST_SYNONYMABLE_FILES}
	@echo
	@echo DEST_SYNONYM_FILES=${DEST_SYNONYM_FILES}
	@echo
