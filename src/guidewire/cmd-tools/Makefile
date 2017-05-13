FROMDIR=${HOME}/lib/guidewire/$(shell pwd | xargs basename | sed 's/-internal$$//')

FILES+=$(shell find ${FROMDIR} -type f -printf '%f ')

#debug:
#	@echo FROMDIR=${FROMDIR}
#	@echo FILES=${FILES}

all:	${FILES}

${FILES}:
	ln -s ${FROMDIR}/$@

