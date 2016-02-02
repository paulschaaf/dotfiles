all:	day.every/updatedb hour.every/kill_unnecessary_processes
	crontab crontab

hour.every/kill_unnecessary_processes:	kill_unnecessary_processes
	ln -s ../$? $@

#day.every/document_system.cron:	document_system.cron
#	ln -s ../$? $@

#day.every/rm_empty_directories:	rm_empty_directories
#	ln -s ../$? $@

#day.every/wishlist:	wishlist
#	ln -s ../$? $@

./day.every/updatedb:	updatedb
	ln -s ../$? $@

minute.every_30/00remove_backups:	00remove_backups
	ln -s ../$? $@

generate_Makefile_source:
	@find \( -type l -o -name '*.lnk' \) -printf '%p:\t%f\n\tln -s ../$$? $$@\n\n'
	@echo