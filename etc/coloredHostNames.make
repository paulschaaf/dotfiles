# get list of hosts for potential connections
# 1) ignore comment lines
# 2) ingore lines with specification 'nomenu'
# 3) choose those with matching domains or the domain .any
# 4) sort the list of names
# 4) put 'localhost' at top of list
# 4) colorize the entries by uname

#all:	.coloredHostNames

#.coloredHostNames:	
domain = $(shell domainname)
#exclusions = `grep "\(^ *#\)\|\(\.nomenu\)" .hosts \
#	| grep -v ".\($(domain)\)\|\(any\)" \
#	| cut -f1 | sort`
hostNames = $(shell grep -v "\(^ *#\)\|\(\.nomenu\)" .hosts | grep ".\(`domainname`\)\|\(any\)" | cut -f1 | sort)
coloredNames = `$HOME/bin/colorHostName localhost $(hostNames)`

coloredHostNames:
	@rm -f $(HOME)/.coloredHostNames
	@echo $(HOSTNAME) hostNames=$(coloredNames)
# >> $(HOME)/.coloredHostNames

debug:
	@echo $(hostNames)
	@echo $(domain)
