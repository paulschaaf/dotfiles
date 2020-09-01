HOSTS=copernicus PSCHAAF-MBP15.guidewire.com

.PHONY:	all clean allowUserWrite cleanLinks disallowAllWrites

all: allowUserWrite ${HOSTS} disallowAllWrites

clean:	allowUserWrite cleanLinks disallowAllWrites

allowUserWrite:
	@chmod u+w .

disallowAllWrites:
	@chmod a-w .

${HOSTS}:
	ln -s ssh-to $@

cleanLinks:
	rm -f ${HOSTS}
