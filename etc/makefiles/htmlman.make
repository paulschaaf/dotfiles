# comment this next line out to use bullets instead of numbers
NUMBER_PAGE_LINKS=true

TECHREF=/home/pschaaf/doc/TechRef

MANPAGES=apropos bash bison bzip2 chattr clisp cpio csh cvs date dd diff emacs enscript expect expr fdisk file find flex fstab gawk gcc gnuclient gpg grep groff grub init info inittab jar java javac kill ksh less links lilo ls lsattr lynx make make_manual man more mount ncftp nice perl plucker-build pnm ps python renice rman rsync ruby scp screen sed sftp signal.7 slocate ssh ssh-add ssh-agent ssh-keygen ssh_config ssh-copy-id ssh-keysign sshd startx sudo sudoers tar telinit terminfo test tr updatedb urpmi urpmi.addmedia.8 urpmi.removemedia.8 urpmi.update.8 whatis X XFree86 Xsecurity Xserver xauth xdm xemacs xinetd xinit xterm zsh

NEEDS_MAN2HTML=gcc perl

PDB_CATEGORY=TechRef
PDB_TITLE=man pages

ifdef NUMBER_PAGE_LINKS
LIST_TAG=ol
else
LIST_TAG=ul
endif

FIND_LOCAL_HTML_FILES_CMD=find -type f -maxdepth 1 -name '*.html' ! -regex './\(index\|manpages\).html'

PREEXISTING_LOCAL_HTML_FILES=$(shell ${FIND_LOCAL_HTML_FILES_CMD} -printf '%f ')

ENSCRIPT=enscript --lang=html --toc -o -
TREE=tree -L 1 -P '*.html' -I '$@' -H '.'

PYTHON=/usr/bin/python
PYTHON_OPTIONS=-W ignore::FutureWarning -W ignore::DeprecationWarning

PLUCKER=/usr/bin/plucker-build
PLUCKER_OPTIONS=--category=${PDB_CATEGORY} --launchable --doc-name="$$(basename $@)" --title="$(basename $$@)"

PYTHON_CMD=${PYTHON} ${PYTHON_OPTIONS}
PLUCKER_CMD=${PYTHON_CMD} ${PLUCKER} ${PLUCKER_OPTIONS}

# Create a new empty file, and rename it to the argument filename. This
# avoids cracker tricks like using a symlink or race condition to redirect
# the overwrite to a different (important) file. See man mktemp.
MKTEMP=mktemp $$$$.XXXXXX
SAFELY_MAKE=mv $$(${MKTEMP})
SAFE_TARGET=$$(rm -f $@; ${SAFELY_MAKE} $@; echo $@)

# ======================= general targets

.PHONY:	pdb
.SUFFIXES:

all:	manpages.pdb TRAMP-Manual.pdb zsh_help.pdb index.html
rebuild:	clean all
clean:
	rm -f *.html *.pdb

.DEFAULT:
	@make -s $@.html

# ======================= pdb database

%.pdb:	%.html
	@echo
	${PLUCKER_CMD} file://$(shell pwd)/$? >> ${SAFE_TARGET}

# ======================= man pages

TARGET_MAN_SECTION_AND_PAGE=$(subst ., ,$(suffix $(basename $@))) $(basename $(basename $@))
TARGET_MANPAGE=$$(man --path${TARGET_MAN_SECTION_AND_PAGE})
CAT_TARGET_MANPAGE=make -s ${TARGET_MANPAGE}

%.1 %.2 %.3 %.4 %.5 %.6 %.7 %.8 %.9:	.PHONY
	cat $@

%.bz2:	.PHONY
	bzcat $@

%.gz:	.PHONY
	zcat $@

# ======================= html pages

zsh_help.html:
	@echo
	(cd ${HELPDIR}; ${ENSCRIPT} *) >> ${SAFE_TARGET}

zsh/zsh_toc.html:
	ln -fs ${TECHREF}/linux/zsh/manual $(subst /,,$(dir $@))

zsh.html:	zsh/zsh_toc.html
	@echo
	sed 's,HREF="z,HREF="zsh/z,g' $? >> ${SAFE_TARGET}

make_manual/make.html:
	ln -fs ${TECHREF}/linux/make $(subst /,,$(dir $@))

make_manual.html:	make_manual/make.html
	@echo
	cat $? >> ${SAFE_TARGET}

$(addsuffix .html,${NEEDS_MAN2HTML}):
	${CAT_TARGET_MANPAGE} | man2html >> ${SAFE_TARGET}

%.html:
	@echo
	${CAT_TARGET_MANPAGE} | (rman -f html 3>&1 >> ${SAFE_TARGET} 2>&3 3>&-) >/dev/null

index.html:	manpages.html
	@ln -fs $? $@

manpages.html:	$(addsuffix .html,${MANPAGES}) ${PREEXISTING_LOCAL_HTML_FILES}
	@echo
	@echo Building ${SAFE_TARGET}
	@echo '<html><head><title>'${PDB_TITLE}'</title></head>' >> $@
	@echo '<body><h1>'$(basename $@)'</h1>' >> $@
	@echo '<${LIST_TAG}>' >> $@
	@${FIND_LOCAL_HTML_FILES_CMD} -printf '<li><a href="%f">%f\n' | sort -u | sed 's,\.html$$,</a>,g' >> $@
	@echo '</${LIST_TAG}>' >> $@
ifndef NUMBER_PAGE_LINKS
	@echo Referenced $$(grep -ic 'href=' $@) manual pages. >> $@
endif
	@printf '<p><i>Last updated by <a href="mailto:%s@%s">%s</a> on %s.</i>\n' `whoami` `hostname` `whoami` "`date +%c`" >> $@
	@echo '</body></html>' >> $@

# ======================= debug

debug:
	@echo '$$0='$$0; echo '$$1='$$1; echo '$$_='$$_; echo '$$*='$$*
	@echo '$$(basename /usr/share/man/man1/zsh.1.bz2)'=$(basename /usr/share/man/man1/zsh.1.bz2)
	@echo '$$(suffix /usr/share/man/man1/zsh.1.bz2)'=$(suffix /usr/share/man/man1/zsh.1.bz2)
