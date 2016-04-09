-include build.properties
-include ~/lib/debugging.Makefile
-include ~/lib/DOS_colors.make
-include bin/build.properties
-include modules/ant/build.properties
#-include env.sh  # recursive property PATH


# ================================================
# ======= User Configurable Variables

APP_ID	           ?= $(shell basename `pwd` | grep --silent '^TrainingApp' && echo TA || echo ${product.name.upper})

BACKUP_CONFIG_FILES ?= true
BACKUP_FILE_EXTN    := bak
JAVA_PROCESS_ID_TAG := INFO.process
TOOLBAR_DIRNAME     := Windows-Toolbar
LINKS_DIR            = ~/Favorites/${dev.server.port}
MS_IE               := /c/PROGRA~1/INTERN~1/iexplore.exe

# ANT
ANT_HOME_1.6.5      := /c/ant/apache-ant-1.6.5
ANT_HOME_1.7.1      := /c/ant/apache-ant-1.7.1

# JAVA
JAVA_HOME_1.5.0     := /c/PROGRA~1/java/jdk1.5.0_14
JAVA_HOME_1.6.0     := /c/PROGRA~1/java/jdk1.6.0_14

# Memory Settings

# SERVER_MEMORY_CC := -Xms768m -Xmx768m -XX:MaxPermSize=128m -XX:PermSize=128m
SERVER_MEMORY_AB 	  := -Xmx256m -XX:MaxPermSize=64m
SERVER_MEMORY_BC 	  := ${SERVER_MEMORY_AB}
SERVER_MEMORY_CC 	  := -Xmx480m -XX:MaxPermSize=128m
SERVER_MEMORY_PC 	  := ${SERVER_MEMORY_CC}
SERVER_MEMORY_PX 	  := ${SERVER_MEMORY_AB}
SERVER_MEMORY_TA 	  := ${SERVER_MEMORY_AB}

# STUDIO_MEMORY_AB := -Xms512m -Xmx512m -XX:MaxPermSize=128m -XX:PermSize=128m
STUDIO_MEMORY_AB 	  := -Xmx512m -XX:MaxPermSize=128m
STUDIO_MEMORY_BC 	  := ${STUDIO_MEMORY_AB}
STUDIO_MEMORY_CC 	  := ${STUDIO_MEMORY_AB}
STUDIO_MEMORY_PC 	  := ${STUDIO_MEMORY_CC}
STUDIO_MEMORY_PX 	  := ${STUDIO_MEMORY_AB}
STUDIO_MEMORY_TA 	  := ${STUDIO_MEMORY_AB}

# PORT
SERVER_PORT_AB      := 8280
SERVER_PORT_BC      := 8580
SERVER_PORT_CC      := 8080
SERVER_PORT_PC      := 8180
SERVER_PORT_PX      := 8380
SERVER_PORT_TA      := 8880


# ======= DOS Colors use 2 codes, background then foreground, chosen from
# DOS_BLACK,  DOS_GRAY,         DOS_BLUE,    	 DOS_LIGHT_BLUE,
# DOS_GREEN,  DOS_LIGHT_GREEN,  DOS_CYAN,    	 DOS_LIGHT_CYAN,
# DOS_RED,    DOS_LIGHT_RED,    DOS_PURPLE,  	 DOS_MAGENTA,
# DOS_BROWN,  DOS_YELLOW,       DOS_LIGHT_GRAY,  DOS_WHITE
SERVER_COLOR_AB     := ${DOS_GRAY}${DOS_WHITE}
SERVER_COLOR_BC     := ${DOS_BROWN}${DOS_WHITE}
SERVER_COLOR_CC     := ${DOS_BLUE}${DOS_WHITE}
SERVER_COLOR_PC     := ${DOS_PURPLE}${DOS_WHITE}
SERVER_COLOR_PX     := ${DOS_CYAN}${DOS_WHITE}
SERVER_COLOR_TA     := ${SERVER_COLOR_AB}
SERVER_COLOR_       := ${DOS_BLACK}${DOS_RED}    # If all else fails


# ======= Source code management
# Comment out SCM to disable Studio access to scm
SCM                 := svn
SCM_EXE             := /c/PROGRA~1/SlikSvn/bin/svn.exe

# Should checksum should ignore SCM files under the modules dir?
# If true you can use SCM for those files, even if you do not
# enable SCM access through Studio.
#
# true  - checksum ignores files that match the pattern
# false - checksum run against every file under modules/
USE_CHECKSUM_IGNORE_REGEX := true


# Files that match are ignored by the checksum checker
SCM_REGEX_cvs       := (^|.*/)CVS(/.*|$)
SCM_REGEX_svn       := (^|.*/)\.svn(/.*|$$)

# Icons
ADMIN_ICON   	      = ${SHELL32DLL} --iconoffset=104
BROWSER_ICON 	      = ${SHELL32DLL} --iconoffset=220
FOLDER_ICON  	      = ${EXPLORER}   --iconoffset=13
STUDIO_ICON  	      = ${SHELL32DLL} --iconoffset=161
STOP_ICON            = ${SHELL32DLL} --iconoffset=133

DEBUG_DISPLAYED_VARS += APP_ID SERVER_ICON product.name.upper


# ======= End of user-configurable variables
# ================================================
# ================================================
# ================================================

# Make sure this is the default target
all:


# ================================================
# ======= Which Platform?

PLATFORM := $(shell \
	if [ -f 'bin/build.properties' ]; then \
		echo BEDROCK; \
	elif [ -d 'webapps' ]; then \
		echo CARBON; \
	elif [ -f 'modules/ant/build.properties' ]; then \
		echo DIAMOND; \
	fi)
DEBUG_DISPLAYED_VARS += PLATFORM


# ================================================
# ======= BEDROCK Platform

ifeq (${PLATFORM}, BEDROCK)
BEDROCK := true
ANT_HOME_CYG     	  := ${ANT_HOME_1.6.5}
BUILD.PROPERTIES 	  := bin/build.properties
BUILD.XML        	  := bin/build.xml
CONFIGURATION    	  := modules/configuration
JAVA_HOME_CYG       := ${JAVA_HOME_1.5.0}
# 2010/02/08 was just PRODUCT_BATCH_FILE   := ant
PRODUCT_BATCH_FILE   = ./gw${APP_ID}.bat
# PRODUCT_BATCH_FILE   = ./gw${product.name}.bat
SERVER_ICON  	      = ${PRODUCT_ROOT}/webapps/${product.name}/resources/Ocean/images/favicon.ico
#  SERVER_ICON		     := ${SHELL32DLL} --iconoffset=137
WORKING_DIR          = ${PRODUCT_ROOT}/bin

-include bin/product-config.properties
-include toolkit-data/bin/server.properties

DEBUG_DISPLAYED_VARS += $(call VARS_FROM, \
	bin/product-config.properties, \
	toolkit-data/bin/server.properties)
endif


# ================================================
# ======= CARBON Platform

ifeq (${PLATFORM}, CARBON)
CARBON           	  := true
ANT_HOME_CYG     	  := ${ANT_HOME_1.7.1}
BUILD.PROPERTIES 	  := modules/ant/build.properties
BUILD.XML        	  := modules/ant/build.xml
CONFIGURATION    	   = modules/configuration
JAVA_HOME_CYG       := ${JAVA_HOME_1.6.0}
PRODUCT_BATCH_FILE   = ./bin/gw${product.name}.bat
#PRODUCT_BATCH_FILE   = ./bin/gw${APP_ID}.bat
SERVER_ICON  	      = ${PRODUCT_ROOT}/webapps/${product.name}/resources/Ocean/images/favicon.ico
#  SERVER_ICON		     := ${SHELL32DLL} --iconoffset=137
WORKING_DIR          = ${PRODUCT_ROOT}

# todo: TEST THIS!
${PRODUCT_BATCH_FILE}:
	@${TRACE} -n 'Fixing batch file ...'
	@grep -q '^call "%~dp0\..\env.bat' $@ \
		&& echo -n ' already' \
		|| (${SED_BACKUP} -e 's/^\(@echo off.*\)/\1\ncall "%~dp0\..\env.bat"\/g' $@ >> $@ && unix2dos $@)
	@echo ' done!'
endif


# ================================================
# ======= DIAMOND Platform

ifeq (${PLATFORM}, DIAMOND)
DIAMOND             := true
ANT_HOME_CYG     	  := ${ANT_HOME_1.7.1}
BUILD.PROPERTIES 	  := modules/ant/build.properties
BUILD.XML        	  := modules/ant/build.xml
CONFIGURATION    	   = modules/configuration
JAVA_HOME_CYG       := ${JAVA_HOME_1.6.0}
PRODUCT_BATCH_FILE   = ./bin/gw${product.name}.bat
#PRODUCT_BATCH_FILE   = ./bin/gw${APP_ID}.bat
SERVER_ICON  	      = ${PRODUCT_ROOT}/modules/${product.name}/webresources/Ocean/images/favicon.ico
#  SERVER_ICON		     := ${SHELL32DLL} --iconoffset=137
WORKING_DIR          = ${PRODUCT_ROOT}

# todo: TEST THIS!
${PRODUCT_BATCH_FILE}:
	@${TRACE} -n 'Fixing batch file ...'
	@grep -q '^call "%~dp0\..\env.bat' $@ \
		&& echo -n ' already' \
		|| (${SED_BACKUP} -e 's/^\(@echo off.*\)/\1\ncall "%~dp0\..\env.bat"\/g' $@ >> $@ && unix2dos $@)
	@echo ' done!'
endif


# ================================================
# ======= All Platforms

-include ${BUILD.PROPERTIES}

DEBUG_DISPLAYED_VARS += BUILD.PROPERTIES WORKING_DIR
DEBUG_DISPLAYED_VARS += $(call VARS_FROM, ${BUILD.PROPERTIES})

export ANT_HOME  	   = $(shell cygpath -aw ${ANT_HOME_CYG})
export JAVA_HOME 	   = $(shell cygpath -aw ${JAVA_HOME_CYG})
export PATH      	  := ${ANT_HOME_CYG}/bin:${JAVA_HOME_CYG}/bin:${PATH}

PRODUCT_BATCH_FILE_W32 = $(shell cygpath -aw ${PRODUCT_BATCH_FILE})
SERVER.PORT          = ${SERVER_PORT_${APP_ID}}
server.url           = http://localhost:${SERVER.PORT}/${product.name}

DEBUG_DISPLAYED_VARS += ANT_HOME APP_ID BUILD.XML CONFIGURATION JAVA_HOME JAVA_HOME_CYG PATH PRODUCT_BATCH_FILE PRODUCT_BATCH_FILE_W32 product.name SERVER.PORT server.url WORKING_DIR


# ================================================
# ======= Computed Variables

# Files
CONFIG.XML          := ${CONFIGURATION}/config/config.xml
LOGGING.PROPERTIES  := ${CONFIGURATION}/config/logging/logging.properties

# Directories
PRODUCT_ROOT        := $(shell pwd)
PRODUCT_ROOT_NODRIVE := $(shell echo ${PRODUCT_ROOT} | sed 's_^/[a-zA-Z]/_/_')
PRODUCT_ROOT_W32 	  := $(shell cygpath -aw ${PRODUCT_ROOT})
SYSTEMROOT_CYG   	  := $(shell cygpath -au $${SYSTEMROOT})

DEBUG_DISPLAYED_VARS += ANT PRODUCT_ROOT PRODUCT_ROOT_W32 PRODUCT_ROOT_NODRIVE SYSTEMROOT_CYG

# External Utilities
ANT 				  	  := $(shell cygpath -au '${ANT_HOME}'/bin/ant)
CMD 				  	  := $(shell cygpath -au $${COMSPEC})
EXPLORER         	  := ${SYSTEMROOT_CYG}/explorer.exe
SHELL32DLL       	  := ${SYSTEMROOT_CYG}/system32/SHELL32.dll


# Product Information

# ==========
# This variable is the source of many of the product version-related variables
# ==========
PRODUCT_FULL_NAME    = $(shell cd ${WORKING_DIR}; ${PRODUCT_BATCH_FILE} version | sed -ne '/Center [1-9]/ {s/^[^]]*] //p}')
# ==========
# ==========


MAJOR_VERSION        = $(shell echo ${PRODUCT_VERSION} | sed -ne 's/\([0-9]\.[0-9]\).*/\1/p')
product.desc        ?= $(shell echo ${PRODUCT_FULL_NAME} | sed 's/ [1-9].*//g')
product.name.upper  ?= $(shell echo ${gw.app} | tr '[:lower:]' '[:upper:]')
#product.name.upper  ?= $(shell echo ${product.desc} | sed 's/[a-z]//g')
product.name        ?= ${gw.app}

PRODUCT_DESCRIPTION  = ${APP_ID} ${PRODUCT_VERSION} (${PRODUCT_ROOT_W32})
PRODUCT_VERSION     := $(subst ${product.desc} ,,${PRODUCT_FULL_NAME})

TRAINING_APP        := $(shell basename `pwd` | grep --silent '^TrainingApp' && echo true || echo false)

DEBUG_DISPLAYED_VARS += MAJOR_VERSION PRODUCT_DESCRIPTION PRODUCT_FULL_NAME product.desc PRODUCT_VERSION TRAINING_APP


# Configuration
BACKUP_EXTENSION 	  := ${subst ..,,${if ${BACKUP_CONFIG_FILES},.${BACKUP_FILE_EXTN}}}
SERVER_MEMORY    	  := -D${JAVA_PROCESS_ID_TAG}="${APP_ID} ${PRODUCT_VERSION} Server" ${SERVER_MEMORY_${APP_ID}}
STUDIO_MEMORY    	  := -D${JAVA_PROCESS_ID_TAG}="${APP_ID} ${PRODUCT_VERSION} Studio" ${STUDIO_MEMORY_${APP_ID}}
TOOLBAR_DIR      	  := ${TOOLBAR_DIRNAME}/${APP_ID}${MAJOR_VERSION}
SED_BACKUP       	  := sed -i${if ${BACKUP_EXTENSION},${BACKUP_EXTENSION}}

DEBUG_DISPLAYED_VARS += SERVER_MEMORY STUDIO_MEMORY TOOLBAR_DIR


# DOS Colors
SERVER_COLOR     	  := ${SERVER_COLOR_${APP_ID}}
DEBUG_DISPLAYED_VARS += SERVER_COLOR


ifeq (${USE_CHECKSUM_IGNORE_REGEX}, true)
SCM_REGEX           := $(if ${SCM},${SCM_REGEX_${SCM}},${SCM_REGEX_svn})
endif

DEBUG_DISPLAYED_VARS += SCM SCM_REGEX


# ================================================
# ======= Logging

# Trailing echo allows sending options to echo command, e.g.
# ${MESSAGE} -n This shows on the same line as the message delimiter
MESSAGE	 ?= echo -n '\# == '; echo
TRACE		 := echo; \
		       echo '\# ================================================'; \
		       ${MESSAGE}
WARN_MSG	 := $(subst =,*,${MESSAGE})
WARN_TRACE:= $(subst =,*,${TRACE})


# ================================================
# ======= Top-level targets

.PHONY:	all always_check_file show_versions

all:	show_versions ${BUILD.XML} Makefile env chmod dictionary toolkit toolbar build.properties

show_versions:
	@echo 'ANT       = ${ANT}'
	@echo 'ANT_HOME  = ${ANT_HOME}'
	@echo 'JAVA_HOME = ${JAVA_HOME}'
	@echo
	@echo; echo -n '# Using '; ${ANT} -version; echo -n '   '; which ant
	@echo; echo -n '# Using '; java -version;   echo -n '   '; which java
	@echo
	@echo "# Product uses Guidewire's ${PLATFORM} platform"
	cd ${WORKING_DIR}; ${PRODUCT_BATCH_FILE} version | sed -ne '/Center [1-9]/ {s/^[^]]*] //p}'
	@echo PRODUCT_FULL_NAME=${PRODUCT_FULL_NAME}

debug:	show_versions ${BUILD.XML}

${BUILD.XML}:
	@echo "ERROR: $@ not found!"
	exit 1

chmod:
	@${TRACE} 'Setting base modules to read-only ...'
	@/usr/bin/find modules -mindepth 1 \
		\( -name '.svn' -o -name '$(notdir ${CONFIGURATION})' -o -name 'ant' \) -prune \
      -o \( -type d ! -perm 555 -printf '   %M\t%p\n' -exec chmod 555 {} + \) \
		-o \( -type f ! -perm 444 -printf '   %M\t%p\n' -exec chmod 444 {} + \)
	@${MESSAGE} 'Done!'
	@${TRACE} 'Setting user modules to writable ...'
	@/usr/bin/find ${CONFIGURATION} $(if ${CARBON},modules/ant) \
		\( -name '.svn' \) -prune \
      -o \( -type d ! -perm 777 -printf '   %M\t%p\n' -exec chmod 777 {} + \) \
		-o \( -type f ! -perm 666 -printf '   %M\t%p\n' -exec chmod 666 {} + \)
	@${MESSAGE} 'Done!'

backup:
	@${TRACE} Backing up modules/configuration
	cd modules; \
	7z a ../${APP_ID}_${PRODUCT_VERSION}_configuration_`date +%F_%H.%M`.7z $(notdir ${CONFIGURATION})

backup_all:
	@${TRACE} Backing up entire configuration
	cd ..; \
	7z a ${product.desc}${PRODUCT_VERSION}_installed.7z $(notdir ${PRODUCT_ROOT})


# ================================================
# ======= Preparation

Makefile:
	ln -fs ../$@ .

dictionary toolkit:
	@make regen-$@

.PHONY:	env regen-dictionary regen-toolkit version
regen-dictionary regen-toolkit:
	cd ${WORKING_DIR}; ${PRODUCT_BATCH_FILE} $@


 studio/project.gpp:
#  <remote-server server-url="http://localhost:8080/${product.name}"/>
#  <external-editors diffTool="C:\Program Files\Araxis\Araxis Merge v6.5\Merge.exe">
#    <external-editor editor-path="C:\Program Files\Liquid Technologies\Liquid XML Studio 2008\XmlStudio.exe" extension="xml"/>
#  </external-editors>

env:	env.bat env.sh

env.sh:
	@(echo '#!/bin/sh'; \
	  echo 'export ANT_HOME=${ANT_HOME_CYG}';   \
	  echo 'export JAVA_HOME=${JAVA_HOME_CYG}'; \
	  echo 'export PATH=$${JAVA_HOME}/bin:$${ANT_HOME}/bin:$${PATH}') >| $@

env.bat:
	@(echo 'SET ANT_HOME=${ANT_HOME}';   \
	  echo 'SET JAVA_HOME=${JAVA_HOME}'; \
	  echo 'SET PATH=%JAVA_HOME%\bin;%ANT_HOME%\bin;%PATH%')          >| $@
	@unix2dos $@ 2>&1 | sed '/${@}: done.$$/d'

version:	env
	cd ${WORKING_DIR}; ${PRODUCT_BATCH_FILE} version

# ================================================
# ======= build.properties

build.properties:	${BUILD.PROPERTIES}
${BUILD.PROPERTIES}:
	@${TRACE} -n 'Server memory settings ...'
	@grep -q '^dev.server.memory.args = ${SERVER_MEMORY}' $@ \
		&& echo -n ' already' \
		|| (${SED_BACKUP} -e 's/^\(dev.server.memory.args\)/#\1/g' $@; \
			echo; echo 'dev.server.memory.args = ${SERVER_MEMORY}' >> $@)
	@echo ' set!'

	@${TRACE} -n 'Studio memory settings ...'
	@grep -q '^studio.memory.args = ${STUDIO_MEMORY}' $@ \
		&& echo -n ' already' \
		|| (${SED_BACKUP} -e 's/^\(studio.memory.args\)/#\1/g' $@; \
			echo 'studio.memory.args = ${STUDIO_MEMORY}' >> $@)
	@echo ' set!'

ifdef SCM_REGEX
	@${TRACE} -n 'SCM regex ...'
	@grep -q '^scm.regex = ${SCM_REGEX}' $@ \
		&& echo -n ' already' \
		|| (${SED_BACKUP} -e '/^scm.regex/d' $@; \
			echo 'scm.regex = ${SCM_REGEX}' >> $@)
	@echo ' set!'
endif


# ================================================
# ======= logging.properties

.PHONY:	logging.properties ${LOGGING.PROPERTIES}

logging.properties:	${LOGGING.PROPERTIES}
${LOGGING.PROPERTIES}:
	@${TRACE} -n 'Setting logging directory ...'
	@newValue='.File=${PRODUCT_ROOT_NODRIVE}/logs/'; \
	grep -q "$${newValue}" $@ \
		&& echo -n ' already' \
		|| ${SED_BACKUP} -e \
			"s;\\.File=.*/\\(.*\\);$${newValue}\\1;" $@
	@echo ' done!'


# ================================================
# ======= config.xml

.PHONY:	config.xml ${CONFIG.XML}

config.xml:	${CONFIG.XML}
${CONFIG.XML}:
	@${TRACE} -n 'Enable Internal Tools ...'
	@grep -q 'name="EnableInternalDebugTools" value="true"' $@ \
		&& echo -n ' already' \
		|| ${SED_BACKUP} -e \
			'/name="EnableInternalDebugTools" value="false"/ { s/false/true/ }' $@
	@echo ' done!'

	@${TRACE} -n 'Enable Incremental PCF compilation ... '
	@grep -q 'name="PCFVerificationMode" value="incremental"' $@ \
		&& echo -n ' already' \
		|| ${SED_BACKUP} -e \
			'/name="PCFVerificationMode" value="/ { s/value="[^"]*"/value="incremental"/ }' $@
	@echo ' done!'

	@${TRACE} -n 'Changing database location ...'
	@newValue='h2:file:${PRODUCT_ROOT_NODRIVE}/db/'; \
	grep -q "$${newValue}" $@ \
		&& echo -n ' already' \
		|| ${SED_BACKUP} -e "s;h2:file:[^\"]*/;$${newValue};g" $@
	@echo ' done!'

DEBUG_DISPLAYED_VARS += CONFIG.XML

# ================================================
# ======= Links

.PHONY:	toolbar cmd directory login Login\ screen screen server STOP studio zsh
toolbar:	cmd directory Login\ screen login server STOP studio screen zsh

${TOOLBAR_DIR}:
	@mkdir -p $@

cmd directory login screen zsh: %:	${TOOLBAR_DIR}/%.lnk
# Login\ screen:	${TOOLBAR_DIR}/Login\ screen.url
server studio STOP:	%:	${TOOLBAR_DIR}/${APP_ID}\ ${PRODUCT_VERSION}\ %.lnk

${TOOLBAR_DIR}/cmd.lnk:	${TOOLBAR_DIR}
	@${TRACE} Making link $(basename $@)
	mkshortcut --name='${@}'			  	  \
		--desc='${PRODUCT_DESCRIPTION}'    \
		--workingdir='${PRODUCT_ROOT}/bin' \
		--arguments='/T:${SERVER_COLOR} /K title ${APP_ID} ${PRODUCT_VERSION}' \
		/c/WINDOWS/system32/cmd.exe
	@chmod a+rw '${@}'


${TOOLBAR_DIR}/directory.lnk:	${TOOLBAR_DIR}
	@${TRACE} Making link $(basename $@)
	@mkshortcut --name='${@}'			  \
		--desc='${PRODUCT_DESCRIPTION}' \
		--icon=${FOLDER_ICON}           \
		--arguments='/e,/root,${PRODUCT_ROOT_W32}' \
		${EXPLORER}
	@chmod a+rw '${@}'


# BTW if you simply name this file "login.url" Windows XP won't display
# the icon correctly! P.G. Schaaf 2009/06/23
# ${TOOLBAR_DIR}/Login\ screen.url:	${TOOLBAR_DIR}
# 	@${TRACE} Making link $(basename $@)
# 	@(echo '[InternetShortcut]'; \
# 	  echo 'URL=${server.url}';  \
# 	  echo 'IDList=';            \
# 	  echo 'IconFile=${BROWSER_ICON}') >| '${@}'
# 	@unix2dos '${@}'
# 	@chmod a+rw '${@}'
# 	  # echo 'IconFile=$(shell cygpath -asw ${SERVER_ICON})'; \
# 	  # echo 'IconIndex=1')        >| '${@}'


${TOOLBAR_DIR}/login.lnk:	${TOOLBAR_DIR}
	@${TRACE} Making link $(basename $@)
	@mkshortcut --name='${@}'          \
		--desc='${PRODUCT_DESCRIPTION}' \
		--workingdir='${WORKING_DIR}'   \
		--icon=${BROWSER_ICON}          \
		--arguments='${server.url}'     \
		${MS_IE}
	@chmod a+rw '${@}'


${TOOLBAR_DIR}/${APP_ID}\ ${PRODUCT_VERSION}\ server.lnk:	${TOOLBAR_DIR}
	@${TRACE} Making link $(basename $@)
	@mkshortcut --name='${@}'          \
		--desc='${PRODUCT_DESCRIPTION}' \
		--workingdir='${WORKING_DIR}\bin' \
		--icon=${SERVER_ICON}           \
		--arguments='/T:${SERVER_COLOR} /K ${PRODUCT_BATCH_FILE_W32} -D${JAVA_PROCESS_ID_TAG}="Ant ${APP_ID} ${PRODUCT_VERSION}" -Dgwdebug=true dev-stop dev-start' \
		${CMD}
# Maybe add '& exit' to the command?
	@chmod a+rw '${@}'


${TOOLBAR_DIR}/${APP_ID}\ ${PRODUCT_VERSION}\ STOP.lnk:	${TOOLBAR_DIR}
	@${TRACE} Making link $(basename $@)
	@mkshortcut --name='${@}'          \
		--desc='STOP ${PRODUCT_DESCRIPTION}' \
		--workingdir='${WORKING_DIR}\bin' \
		--icon=${STOP_ICON}           \
		--arguments='/T:${SERVER_COLOR} /K ${PRODUCT_BATCH_FILE_W32} dev-stop && exit' \
		${CMD}
	@chmod a+rw '${@}'


${TOOLBAR_DIR}/${APP_ID}\ ${PRODUCT_VERSION}\ studio.lnk:	${TOOLBAR_DIR}
	@${TRACE} Making link $(basename $@)
	@mkshortcut --name='${@}'          \
		--desc='${PRODUCT_DESCRIPTION}' \
		--workingdir=${WORKING_DIR}     \
		--icon=${STUDIO_ICON}           \
		--arguments='/T:${SERVER_COLOR} /K ${PRODUCT_BATCH_FILE_W32} -D${JAVA_PROCESS_ID_TAG}="Ant ${APP_ID} ${PRODUCT_VERSION} studio" studio \& exit' \
		${CMD}
	@chmod a+rw '${@}'


${TOOLBAR_DIR}/screen.lnk:	${TOOLBAR_DIR}
	@${TRACE} Making link $(basename $@)
	@mkshortcut --name='${@}'          \
		--desc='${PRODUCT_DESCRIPTION}' \
		--show=min                      \
		--workingdir='${PRODUCT_ROOT}'  \
		--icon=/cygwin.ico              \
		--arguments="sh -c \"screenin -t '$$|${APP_ID}${MAJOR_VERSION}:'\"" \
		/bin/run
	@chmod a+rw '${@}'


${TOOLBAR_DIR}/zsh.lnk:	${TOOLBAR_DIR}
	@${TRACE} Making link $(basename $@)
	@mkshortcut --name='${@}'          \
		--desc='${PRODUCT_DESCRIPTION}' \
		--workingdir='${PRODUCT_ROOT}'  \
		--icon=/bin/zsh.exe             \
		--arguments='-i'                \
		/bin/zsh
	@chmod a+rw '${@}'



# ================================================
# ======= Logins

# LOGINS = aapplegate wmotley

# .PHONY:	logins ${LOGINS} admin
# logins:	${LOGINS}

# ${LOGINS} admin: %:	${LINKS_DIR}/%.lnk
# $(addprefix ${LINKS_DIR}/,$(addsuffix .lnk,${LOGINS})):	${LINKS_DIR}
# 	@user=$(basename $(notdir $@)); \
# 	${TRACE} Making link \"$$user\"; \
# 	mkshortcut --show=norm                   \
# 		--name=${@} \
# 		--icon=${BROWSER_ICON} \
# 		--desc="${APP_ID} ${PRODUCT_VERSION} $$user" \
# 		--arguments="\"${server.url}/Login.do?loginPassword=gw&loginName=$$user\"" \
# ${EXPLORER}

# ${LINKS_DIR}:
# 	@${TRACE} Making directory
# 	mkdir -p $@

# ${LINKS_DIR}/admin.lnk:
# 	@user=\"${basename $@}\"; \
# 	echo; echo \# Making link \"$$user\"; \
# 	mkshortcut --show=norm --name '${APP_ID} ${PRODUCT_VERSION} $@' --icon=${ADMIN_ICON} --desc="Login as $$user" --arguments="\"${server.url}/Login.do?loginPassword=gw&loginName=${basename $@}\"" ${EXPLORER}


######
###### Sample `make debug`
######
######
######

# DEBUG: Printing every variable named in DEBUG_DISPLAYED_VARS
# MAJOR_VERSION          = 2.1                                                  # PRODUCT_DESCRIPTION    = BC 2.1.2.8 (c:\GWTraining\BillingCenter)
# PRODUCT_FULL_NAME      = BillingCenter 2.1.2.8                                # PRODUCT_ROOT           = /c/GWTraining/BillingCenter
# PRODUCT_ROOT_W32       = c:\GWTraining\BillingCenter
# PRODUCT_ROOT_NODRIVE   = /GWTraining/BillingCenter
# PRODUCT_VERSION        = 2.1.2.8                                              # SCM                    = svn
# SCM_REGEX              = (^|.*/)\.svn(/.*|$)                                  # SERVER_COLOR           = 2F
# SERVER_MEMORY          = -DINFO.process="BC 2.1.2.8 Server" -Xms256m -Xmx256m 
-XX:MaxPermSize=64m  -XX:PermSize=64m
# STUDIO_MEMORY          = -DINFO.process="BC 2.1.2.8 Studio" -Xms512m -Xmx512m 
-XX:MaxPermSize=128m -XX:PermSize=128m
# SYSTEMROOT_CYG         = /c/WINDOWS                                           # TOOLBAR_DIR            = Windows-Toolbar/BC2.1
# dev.server.port        = 8580
# dev.server.stop.port   = 8579
# ear.name               = BillingCenter
# product.desc           = BillingCenter
# product.name           = bc
# server.url             = http://localhost:8580/bc
# server.user            = su
# war.name               = bc

######################################################
# 8 $ ANT_HOME=/c/ant/apache-ant-1.7.1 /c/ant/apache-ant-1.7.1/bin/ant -f modules/ant/build.xml version
# Buildfile: modules\ant\build.xml

# antcontrib-tasks:

# asserts:

# gw-macros:

# gw-tasks:

# init:
#      [echo] Java version: 1.5.0_15
#      [echo]  Ant version: Apache Ant version 1.7.1 compiled on June 27 2008
# [gw:gscriptInit] 'p4' is not recognized as an internal or external command,'p4' is not recognized as an internal or external command,
# [gw:gscriptInit]
# [gw:gscriptInit] operable program or batch file.operable program or batch file.
# [gw:gscriptInit]
# [gw:gscriptInit] Dev root: C:\GWTraining\PolicyCenter4.0.0.72
# [gw:gscriptInit]   Branch:

# version:
# [gw:gscript] PolicyCenter 4.0.0.72

# BUILD SUCCESSFUL
# Total time: 4 seconds


######################################################
# 17:28 pschaaf@TOLSON /c/GWTraining/PolicyCenter4.0.0.72
# 9 $ (cd ../PolicyCenter; ant -f bin/build.xml version)
# Buildfile: bin\build.xml

# version:
#     [unjar] Expanding: C:\GWTraining\PolicyCenter\dist\gw-pc-version.jar into C:\GWTraining\PolicyCenter\tmp
#      [echo] PolicyCenter 3.0.2.91
#    [delete] Deleting: C:\GWTraining\PolicyCenter\tmp\gw-version.properties

# BUILD SUCCESSFUL
# Total time: 1 second
