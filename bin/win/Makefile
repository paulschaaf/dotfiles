.SUFFIXES:

-include build.properties
-include ~/lib/debugging.Makefile

DEBUG_DISPLAYED_VARS += CYGROOT_MIXED HOME

# ================================================
# ======= System tools
FIND      = /usr/bin/find
NOHUP     = /usr/bin/nohup
REGTOOL   = /usr/bin/regtool
RUN       = /usr/bin/run
SHELL     = /bin/zsh
START     =	/usr/bin/cygstart --hide
XTERM     =	/usr/bin/xterm

SYSTEMROOT     = $(shell cygpath -au $$SYSTEMROOT)
SYSTEM32       = ${SYSTEMROOT}/system32
SHELL32DLL     = ${SYSTEM32}/SHELL32.dll

WIN_SETENV = ${REGTOOL} set -s '/machine/SYSTEM/CurrentControlSet/Control/Session Manager/Environment/'
WIN_SETUSERENV = ${REGTOOL} set -s '/users/Environment/'

# HOME=$(shell cygpath -asw "$$HOMEPATH")
CYGROOT_MIXED=$(shell cygpath --mixed --absolute $$CYGROOT)
CYGROOT_WIN=$(shell cygpath --windows --absolute $$CYGROOT)
SCREENDIR=/tmp

# Pathname transformations
AS_WIN_NAME=cygpath -amf - | sed 's!\<${CYGROOT_MIXED}!%CYGROOT%!; s!/!\\!g'
AS_WIN_PATH=$(subst cygpath -p,cygpath,${AS_WIN_NAME})

ECHO=echo '=== '

# Linking files
LN=@${ECHO} Creating link $@; ln -s
LN_DEFAULT=cd $(@D); ${LN} $? $(@F)
LN_CYGWIN=echo; ${ECHO} Creating link $@; mkshortcut --workingdir '%USERPROFILE%' --icon /cygwin.ico  --name $@
DEBUG_DISPLAYED_VARS += LN_CYGWIN LN_DEFAULT

# Desktops
DEBUG_DISPLAYED_VARS += ALL_DESKTOP MY_DESKTOP
ALL_DESKTOP=$(shell cygpath --windows --short --desktop --all | cygpath --unix -f -)
MY_DESKTOP=$(shell  cygpath --windows --short --desktop       | cygpath --unix -f -)

# Startup folder
DEBUG_DISPLAYED_VARS += ALL_STARTUP
ALL_STARTUP=$(dir ${ALL_DESKTOP})STARTM~1/Programs/Startup


# ================================================
# ======= Logging

# Trailing echo allows sending options to echo command, e.g.
# ${MESSAGE} -n This shows on the same line as the message delimiter
MESSAGE	 ?= echo -n '\# == '; echo
TRACE		  = echo; \
		       echo '\# ================================================'; \
		       ${MESSAGE}
WARN_MSG	  = $(subst =,*,${MESSAGE})
WARN_TRACE = $(subst =,*,${TRACE})


# ================================================
# ======= Applications
DEBUG_DISPLAYED_VARS += CYGPATH EMACS GNUCLIENT NOHUP RUN RUN_WIN XEMACS XTERM

CYGPATH     = ~/bin/win:/local/bin:/usr/local/bin:/bin:/usr/bin:/usr/local/sbin:/usr/sbin:/sbin:/usr/X11R6/bin:~/bin

EMACS       = $(shell which emacs)
GNUCLIENT   = $(shell which cyg-emacsclient.exe)
REGTOOL_WIN = $(shell echo ${REGTOOL} | ${AS_WIN_PATH})
RUN_WIN     = $(shell echo ${RUN} | ${AS_WIN_PATH})
# XEMACS    = $(shell which xemacs)
XTERM_WIN   = $(shell echo ${XTERM} | ${AS_WIN_PATH})


# ================================================
# ======= Top-level targets
.PHONY:	all install removeBackups

all:	removeBackups system_links all_user_links home_links
	htmlize

install:	sysenv userenv registry makewhatis updatedb setup_daemons

removeBackups:
	@${ECHO} Removing backups.
	-@cd; ${FIND} . bin -maxdepth 1 \( -regex '.*~$$' -o -name 'nohup.out' \) -exec rm {} \;


# ================================================
# ======= System links
# SYSTEM_LINKS=/bin/cmd /bin/taskkill /etc/hosts /etc/init.d /etc/networks /etc/protocols /etc/services

# .PHONY:	system_links

# system_links:	${SYSTEM_LINKS}

# /bin/cmd:	${SYSTEM32}/cmd.exe
# /bin/taskkill:	${SYSTEM32}/taskkill.exe
# /etc/win32:	${SYSTEM32}/drivers/etc

# /bin/cmd /bin/taskkill /etc/win32:
# 	${LN_DEFAULT}

# /etc/init.d:	/etc/rc.d
# /etc/hosts /etc/networks /etc/protocols /etc/services:	/etc/win32

# /etc/init.d /etc/hosts /etc/networks /etc/protocols /etc/services:
# 	cd $(@D); ln -s $(subst $(@D)/,,$(?F))/$(subst hosts,lmhosts,$(@F)) $(@F)


# ================================================
# ======= XEmacs links
# .PHONY:	xemacs
# xemacs:	${ALL_DESKTOP}/XEmacs ${HOME}/SendTo/XEmacs registry_openWithGnuclient


# ================================================
# ======= User links
.PHONY:	all_user_links

all_user_links:	${ALL_STARTUP}/new-ssh-agent.sh ${HOME}/bin/win/find

${HOME}/bin/win/find:
	ln -s /usr/bin/find $@

# ${ALL_DESKTOP}/XEmacs:	${XEMACS}
# 	${LN_CYGWIN} --icon $(basename $?).ico --arguments "bash -c ${GNUCLIENT}"      ${RUN}

${ALL_DESKTOP}/Emacs:	${EMACS}
	${LN_CYGWIN} --icon ~/local/sync/images/icons/xemacs.ico --arguments "${SHELL} -lc 'nohup emacs &'" ${RUN}

${ALL_DESKTOP}/KillNonresponsive.lnk:
	@mkshortcut                                \
		--name='${@}'                           \
		--desc='Kills all "not responding" tasks' \
		--icon=${SHELL32DLL} --iconoffset=131   \
		--arguments='/f /fi "status eq not responding"' \
		${SYSTEM32}/taskkill.exe

# todo
#${ALL_STARTUP}/new-ssh-agent.sh:	${HOME}/bin/win/new-ssh-agent.sh
#	${LN_CYGWIN} /usr/bin/sh -a $?
#	run.exe /bin/sh /homes/pschaaf/bin/win/new-ssh-agent.sh
#	${LN_CYGWIN} --arguments '/bin/sh $?' ${RUN}


# ======= $HOME links
.PHONY:	home_links
home_links:	${HOME}/Desktop ${HOME}/SendTo
# ${HOME}/SendTo/XEmacs
# ${HOME}/Desktop/screen

${HOME}/Desktop:	${MY_DESKTOP}
	${LN_DEFAULT}

${HOME}/SendTo:	$(dir ${MY_DESKTOP})/SendTo
	${LN_DEFAULT}

# recode:
#	@echo === Recoding as Unix ASCII.
#	-@recode -v ibmpc..latin1 $(SCRIPTS) 2> /dev/null


# ======= Cygwin links
${HOME}/bin/cygwin:
	@[ -d '${@}' ] || mkdir '${@}'

${HOME}/bin/cygwin/Screen.lnk:	${HOME}/bin/cygwin
	@${TRACE} Making link $(basename $@)
	mkshortcut   					      \
		--name='${@}'			  			\
		--desc=''                     \
		--workingdir='%USERPROFILE%'  \
		--icon=/cygwin.ico            \
		--arguments='-name xscreen -e /usr/bin/screen -R -DD' \
		/usr/bin/rxvt
	@chmod 755 '${@}'

${HOME}/bin/cygwin/zsh.lnk:	${HOME}/bin/cygwin
	@${TRACE} Making link $(basename $@)
	mkshortcut   					      \
		--name='${@}'                 \
		--desc=''                	   \
		--workingdir='%HOMEPATH%'	   \
		--arguments='-il'        	   \
		/usr/bin/zsh
	@chmod 755 '${@}'

${HOME}/bin/cygwin/Emacs.lnk:	${HOME}/bin/cygwin
	@${TRACE} Making link $(basename $@)
	mkshortcut   				 	              \
		--name='${@}'        		           \
		--desc=''            		     	     \
		--workingdir='%HOMEDRIVE%\%HOMEPATH%' \
		/c/PROGRA~1/emacs-22.3/bin/runemacs.exe
	@chmod 755 '${@}'


# ================================================
# ======= System environment variables
.PHONY:	cygpath sysenv

CYGPATH_WIN=$(subst ~,%HOME%,$(shell cygpath -wp ${CYGPATH}))

cygpath:
#	setx CYGPATH '${CYGPATH_WIN}' -m
	${WIN_SETENV}CYGPATH '${CYGPATH_WIN}'
# 'c:\DOCUME~1\pschaaf\bin\win;${CYGROOT_WIN}\usr\local\bin;${CYGROOT_WIN}\bin;${CYGROOT_WIN}\sbin;${CYGROOT_WIN}\usr\sbin;${CYGROOT_WIN}\usr\X11R6\bin'


sysenv:	cygpath
#	setx DISPLAY localhost:0 -m
	${WIN_SETENV}DISPLAY localhost:0~
	${WIN_SETENV}SHELL ${SHELL}


# ================================================
# ======= User environment variables
.PHONY:	userenv

userenv:
#	setx HOME ${HOME}
#	setx PATH %HOME%\\bin
	${WIN_SETENV}DEVMGR_SHOW_NONPRESENT_DEVICES 1
	${WIN_SETUSERENV}HOME ${HOME}
	${WIN_SETUSERENV}PATH %HOME%\\bin


# ================================================
# ======= System Registry
.PHONY:	registry registry_openWithGnuclient showRegistry

registry: registry_openWithGnuclient chere

showRegistry:
	${REGTOOL} get /root/Unknown/shell/openas/command/

registry_openWithGnuclient:	${GNUCLIENT}
	@${ECHO} Setting registry \'Edit in emacsde\' for unknown files
	${REGTOOL} --expand-string set /root/Unknown/shell/Edit in emacs/ '${RUN_WIN} $(notdir $?) "%1"'
	${REGTOOL} get /root/Unknown/shell/open/command/

DRIVE_EXT=/root/Drive/Shell
SHELL_EXT=/root/Directory/Shell
SHELL_UN_EXT=/machine/Software/Microsoft/Windows/CurrentVersion/Uninstall

# Shell Here context menu
chere:	explore_from_here screen_here xterm_here zsh_here
	chere -ilnp -t rxvt

#	${REGTOOL}    add ${DRIVE_EXT}/cygwin_zsh
#	${REGTOOL} -s set ${DRIVE_EXT}/cygwin_zsh/ "&Zsh Here"
#	${REGTOOL}    add ${DRIVE_EXT}/cygwin_zsh/command
#	${REGTOOL} -s set ${DRIVE_EXT}/cygwin_zsh/command/     ${XTERM}

#	${REGTOOL}    add ${SHELL_EXT}/cygwin_zsh
#	${REGTOOL} -s set ${SHELL_EXT}/cygwin_zsh/ "&Zsh Here"
#	${REGTOOL}    add ${SHELL_EXT}/cygwin_zsh/command
#	${REGTOOL} -s set ${SHELL_EXT}/cygwin_zsh/command/ ${XTERM}

#	${REGTOOL}    add /machine/Software/Microsoft/Windows/CurrentVersion/Uninstall/cygwin_zsh
#	${REGTOOL} -s set /machine/Software/Microsoft/Windows/CurrentVersion/Uninstall/cygwin_zsh/DisplayName "Cygwin Zsh Prompt Here"
#	${REGTOOL} -s set /machine/Software/Microsoft/Windows/CurrentVersion/Uninstall/cygwin_zsh/UnInstallString "${CYGROOT_WIN}\bin\sh -c \"/bin/chere -u -s ${SHELL}\""

explore_from_here:
# 	key='${SHELL_EXT}/explore from here'; \
# 	echo Setting "$$key"         'E&xplore From Here'; \
# 	set_registry_key "$$key"         'E&xplore From Here'; \
# 	set_registry_key "$$key/command" 'explorer.exe /e,/root,"%1"'

screen_here:
	chere -iflnr -t cmd -e '&screen here' -s ash -c screenin

# 	key='${SHELL_EXT}/screen here'; \
# 	set_registry_key "$$key"         'Screen Here'; \
# 	set_registry_key "$$key/command" "${RUN_WIN} sh -c 'screenin \"%L\"'"

xterm_here:
	chere -iflnr -t xterm -e '&xterm here' -s zsh

zsh_here:
	chere -ifln -s zsh -e '&zsh here'

# ================================================
# ======= Update databases
.PHONY:	makewhatis updatedb

makewhatis:
	nice -n 19 /usr/sbin/makewhatis

updatedb:
	nice -n 19 $HOME/bin/win/updatedb


# ================================================
# ======= Configuration files:
.PHONY:	setup_daemons setup_init setup_ssh

setup_daemons:	setup_init setup_ssh list_config_scripts

setup_init:
	/bin/init-config

setup_ssh:
	SERVICE_NAME=sshd; \
	SERVICE_PATH=$(which $$SERVICE_NAME); \
	cygrunsrv --install $$SERVICE_NAME --path $$SERVICE_PATH --desc "Cygwin $$SERVICE_NAME daemon" --type auto; \
	cygrunsrv -S $$SERVICE_NAME

list_config_scripts:
# locate all -config scripts, excluding files in the MANPATH
	@echo You may want to run these config files, as well:
	locate -- '-config' | grep -v "^\($(man -w | sed 's,:,\\|,g')\)/"


# ================================================
# ======= Mount Points (mount -m)

# mount -s -b --change-cygdrive-prefix "/"
# mount -f -s -b "c:/cygwin" 		"/"
# mount -f -s -b "c:/cygwin/bin" 	"/usr/bin"
# mount -f -s -b "c:/cygwin/lib" 	"/usr/lib"
# mount -f -s -b "c:/DOCUME~1" 		"/home"
# mount -f -s -b "c:/tmp" 			"/tmp"
