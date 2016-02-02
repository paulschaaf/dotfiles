#.SUFFIXES:	.txt .pdb .html
.PHONY:	install

SOURCE_FILES=$(shell find -name '*.txt' -print)
DATABASES=$(subst txt,pdb,${SOURCE_FILES})

all:	${DATABASES}

debug:
	@echo '$$SOURCE_FILES='$(SOURCE_FILES)
	@echo '$$DATABASES='$(DATABASES)

%.pdb:	%.txt
	@txt2pdbdoc $(basename $@) $< $@

install:	all .install

.install:	${DATABASES}
	@echo; echo ---- Scheduling for install\: $?
	@rsync -u $? ${PDA_INSTALL_DIR} && touch .install

clean:
	@rm -f ${DATABASES} .install

rebuild:	clean all
