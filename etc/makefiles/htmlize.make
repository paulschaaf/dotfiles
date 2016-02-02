FIND_TARGETS=find -type f ! \( -path '*/CVS/*' -o -path '*/win/*' -o -name '.*' -o -name '*~' -o -regex '.*\(core\|index.html\|TAGS\)' -o -regex '.*\.\([0-9]\|bin\|bzip\|bzip2\|bz2\|changes\|dll\|do[ct]\|elc\|exe\|gif\|gnumeric\|gzip\|gz\|image\|o\|pdf\|png\|p[po]t\|ps\|jar\|jpg\|tar\|tgz\|tiff\|sources\|xl[st]\|xpm\|zip\|Z\)' \)
#`cat .htmlize_ignore 2>/dev/null`
TARGETS=$(shell ${FIND_TARGETS} | sed 's-\./--g' | sort)

SOFT_LINK=echo === Symbolically linking $@ to $?; ln -fs $(shell pwd)/$? $@

HTMLIZE=enscript --color --fancy-header --line-numbers=1 --pretty-print --lang=html --toc -o $@
#HTMLIZE=rm -f index.html; tree -H . | sed 's,<a class="DIR" [^>]*>,,g; s,/</a>/,/,g'> index.htm

# Create a new empty file, and rename it to the argument filename. This
# avoids cracker tricks like using a symlink or race condition to redirect
# the overwrite to a different (important) file. See man mktemp.
TEMP_VAR=temp_f
TEMP=$$${TEMP_VAR}

MKTEMP=${TEMP_VAR}=`mktemp $$TMP/$$$$.XXXXXX`
MKTEMPDIR=${TEMP_VAR}=`mktemp -d $$TMP/$$$$.XXXXXX`
RENAME_TO=mv -f ${TEMP}
TEMP_RENAMED_TO=${TEMP}; mv -f ${TEMP}

SAFELY_CAT=${MKTEMP}; cat
INTO=>| ${TEMP_RENAMED_TO}

.PHONY:	maybe_remake_html_targets

all:	maybe_remake_html_targets index.html

maybe_remake_html_targets:
	@${MKTEMP}; \
	echo "${TARGETS}" > ${TEMP}; \
	diff -q ${TEMP} .htmlize_targets 2> /dev/null > /dev/null \
		&& (rm -f ${TEMP}) \
		|| (echo Updating .htmlize_targets; mv -f ${TEMP} .htmlize_targets)

index.html:	.htmlize_targets
	@echo Updating $@
	@rm -f $@
	@${MKTEMP}; ${RENAME_TO} $@
	${HTMLIZE} `cat $<`

debug:
	@echo FIND_TARGETS=${FIND_TARGETS}
	@echo TARGETS=${TARGETS}

clean:
	-rm -f .htmlize_targets index.html

rebuild:	clean all