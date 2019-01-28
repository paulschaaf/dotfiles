HOSTS=copernicus satori satori_home evander d-cc-core d-cc-merge d-cc-stable e-cc-core e-cc-merge e-cc-stable

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
