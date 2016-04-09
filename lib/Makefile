# $Source: e:/MyDocuments/cvsroot/src/Makefile,v $
# $Revision: 1.2 $
# $Date: 2003/08/30 01:14:47 $
# $Author: pschaaf $
# $State: Exp $
# $Name:  $

.PHONY:	chmod

UNAME=$(subst _NT-5.0,,$(shell uname))

all:	$(UNAME)

clean:
	-@rm -f ${LINKS}

rebuild:	clean all

chmod:
	@echo Fixing file permissions.
	@-find -type f ! -regex '.*/CVS/.*' | xargs chmod 644
	@-find -type d ! -name  'CVS'       | xargs chmod 755

CYGWIN:
	htmlize

Linux:	chmod

