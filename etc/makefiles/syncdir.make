SOURCE=$(shell cat SOURCE)
SOURCE_MACHINE=$(firstword $(subst :, ,${SOURCE}))

RSYNC=rsync -avv --cvs-exclude --delete

${HOST}:
	@echo Synching from ${SOURCE} to $@ | lite ${SOURCE} to $@
	${RSYNC} ${SOURCE} . 2>1 | tee syncdir.log

${SOURCE_MACHINE}:
	@echo Nothing to do on $@ since it is the source machine. | lite $@

debug:
	@echo RSYNC=${RSYNC}
	@echo HOST=${HOST}
	@echo SOURCE=${SOURCE}
	@echo SOURCE_MACHINE=${SOURCE_MACHINE}
	@echo "${RSYNC} ${SOURCE} . 2>1 | tee syncdir.log"
