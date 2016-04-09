# ================================================
#
# ClaimCenter Makefile
#
# Paul Schaaf
# 11/30/2004
#
# Run 'make help' for details
#
# TODO:
#     config-override.xml
#
# Note that JMX requires port 1099
#
# 3.0.2 external adapter
# 		<adapter name="PolicySearch" url="http://winnipegosis:9080/adapters/soap/policysearch.wsdl"/>
#
# ================================================

.SUFFIXES:

DEBUG_APPSRVR           ?=
DEBUG_DISPLAYED_VARS    ?=
HELP_DISPLAYED_COMMANDS ?=
TEMP_FILES              ?=


# ================================================
# ======= Configuration File

ifeq ($(shell [ -e config.make ] && echo t), t)
include config.make
else
config.make:
endif

DEBUG_DISPLAYED_VARS  += $(shell awk '/^\#/ {next}; $$2 ~ /=$$/ {print $$1}' config.make)


# ================================================
# ======= Misc. Environment

BROWSER       ?= $(if $CYGROOT,explorer,www)

# CYGROOT is required under CYGWIN
CYGROOT   ?= $(shell [ -e /cygwin.bat ] && cygpath -aw / 2>/dev/null)

CMD-TOOLS ?= cmd-tools$(shell [ -d cmd-tools-internal ] && echo -internal)
STUDIO    ?= studio$(shell [ -d studio-internal ] && echo -internal)
FIND		 ?= /bin/find
HOST      ?= $(shell hostname)
MESSAGE	 ?= echo '\# =='
TRACE		  = echo; \
		       echo '\# ================================================'; \
		       ${MESSAGE}
WARN_MSG	  = $(subst =,*,${MESSAGE})
WARN_TRACE = $(subst =,*,${TRACE})

SCRIPT_FILE_EXT	?= .sh #.$(if $CYGROOT,bat,sh)

APPSRVR_HOST ?= ${HOST}
DEBUG_DISPLAYED_VARS  += APPSRVR_HOST CMD-TOOLS CYGROOT FIND HOST MESSAGE TRACE WARN_MSG WARN_TRACE SCRIPT_FILE_EXT STUDIO


# ================================================
# ======= Application Server Parameters

DEBUG_DISPLAYED_VARS  += APPSRVR_DEBUG_FLAGS JPDA_ADDRESS RUNNING_REMOTELY

ifneq (${HOST}, $(shell hostname))
RUNNING_REMOTELY = ${HOST}
endif

APPSRVR_DEFAULT ?= tomcat
APPSRVR         ?= ${APPSRVR_DEFAULT}

   # ------- Don't touch code in this section -----
     # Define the capitalized app server name as a variable
     #$(shell awk 'BEGIN {print toupper("${APPSRVR}")}') = true
     APPSRVR_UC    = $(shell echo ${APPSRVR} | tr '[a-z]' '[A-Z]')
     ${APPSRVR_UC} = true
	  DEBUG_DISPLAYED_VARS  += ${APPSRVR_UC}
   # ----------------------------------------------

APPLICATION_PORT_DEFAULT      ?= 8080
APPLICATION_PORT              ?= ${APPLICATION_PORT_DEFAULT}
APPSRVR_POLLING_DEFAULT	      ?= 5s
APPSRVR_POLL_REPEAT  	      ?= 3
ifdef TOMCAT
   APPSRVR_START_PORT_DEFAULT ?= 8009
   APPSRVR_DEBUG_FLAGS        ?= -Xdebug -Xrunjdwp:transport=dt_socket,address=8000,server=y,suspend=n
   APPSRVR_STOP_PORT_DEFAULT  ?= 8005

  ifdef DEBUG_APPSRVR
      DEBUG_APPSRVR_ENV=export JPDA_TRANSPORT=${JPDA_TRANSPORT}; export JPDA_ADDRESS=${JPDA_ADDRESS};
      DEBUG_APPSRVR_ARGS=-Xrunjdwp:transport=${JPDA_TRANSPORT},address=${JPDA_ADDRESS},server=y,suspend=n -Xdebug
  endif
endif
ifdef WEBSPHERE
endif


# ================================================
# ======= Project Parameters

PROJECT_DIR=$(shell cygpath -au .)

PROJECT_PKG             = ${PROJECT_NAME}${PROJECT_SUFFIX}
PROJECT_RELEASE		  ?= $(shell awk '/^app.name/ {ANSWER=$$2; exit}; END {print ANSWER}' FS='=' ANSWER=cc config-env/bin/registry.properties)
PROJECT_VERSION		  ?= $(shell awk '/^registry.release/ {ANSWER=$$2; exit}; END {print ANSWER}' FS='=' ANSWER=${PROJECT_RELEASE}3.0 config-env/bin/registry.properties)

DEPLOYABLE_PROJECT_PKG  = config-env/dist/$(subst .,,${PROJECT_SUFFIX})/${PROJECT_PKG}
DEPLOYED_PROJECT_DIR    = ${DEPLOYMENT_DIR}/${PROJECT_NAME}
DEPLOYED_PROJECT_PKG    = ${DEPLOYMENT_DIR}/${PROJECT_PKG}
ISQL                    = isql -S ${DATABASE_SERVER} -U ${DATABASE_USER} -P ${DATABASE_PASSWORD} -d

DEBUG_DISPLAYED_VARS += DEPLOYABLE_PROJECT_PKG DEPLOYED_PROJECT_DIR DEPLOYED_PROJECT_PKG ISQL PROJECT_DIR


# ================================================
# ======= Targets

.PHONY:	all pause_3_sec

all:	${COMPONENT_DIRS} deploy start${APPLICATION_CODE}

LOCATE_DBS += locatedb

help:
	@echo Use the following targets as commands:
	@echo
	@echo ${HELP_DISPLAYED_COMMANDS} | fmt

clean:	undeploy
	-rm -f  ${DEPLOYABLE_PROJECT_PKG} ${TEMP_FILES}
	-rm -rf config-env/tmp

rebuild:	clean all

debug:	config.make
	@make -s _dump_variables

_dump_variables:
	@echo
	@$(foreach varname,\
		$(sort ${DEBUG_DISPLAYED_VARS}),\
		printf "%-24s = %s\n" ${varname} '${${varname}}';)
	@echo

pause_3_sec:
	sleep 3


# ================================================
# ======= Create & Populate Directories

# ifeq (${PROJECT_RELEASE}, cc2.1.5)
# STUDIO=rulecomposer
# START_STUDIO_CMD=run_local${SCRIPT_FILE_EXT}
# else
# STUDIO=studio
# START_STUDIO_CMD=studio.sh
# endif

DEBUG_DISPLAYED_VARS += STUDIO

COMPONENT_DIRS = ${APPSRVR_PATH} ${CMD-TOOLS} config-env doc studio toolkit

# ${APPSRVR_PATH}:	src/${APPSRVR_PATH}.zip
# ${CMD-TOOLS}:	src/${CMD-TOOLS}.zip
# config-env:	src/config-env.zip
# studio:	src/studio.zip
# toolkit:	src/toolkit.zip

unzip:	${APPSRVR_PATH} ${CMD-TOOLS} config-env doc ${STUDIO} toolkit

${CMD-TOOLS} ${APPSRVR_PATH} studio toolkit:
	@${TRACE} Extracting $@
	unzip -qq -d $@ $?

config-env:
	@${TRACE} Extracting $@
	unzip -qq -d $@ $?
	cd $@/config; unzip -qq config-defaults-*.zip

config-env/config:	config-env
	@${TRACE} Extracting $@

config-env/config/config.xml:	config-env
	@make disable_jmx enable_financials enable_internal_tools

LOCATE_DBS += config-env/config/locatedb ${APPSRVR_PATH}/locatedb


# ================================================
# ======= Get new Makefile

.PHONY:	update_Makefile
HELP_DISPLAYED_COMMANDS += update_Makefile

update_Makefile:
ifeq (${HOST}, caledon)
	@echo $@ cannot be performed locally! >&2
	exit 1
else
	scp caledon:~/doc/guidewire/Makefile .
endif


# ================================================
# ======= Edit config.xml

HELP_DISPLAYED_COMMANDS += configure config_setup_sql_server default_to_index.html edit_config enable_financials enable_internal_tools enable_smoke_tests rename_application update_config_files

.PHONY:	configure extra_config update_config_files edit_config config_setup_sql_server enable_financials enable_internal_tools enable_smoke_tests fix_port_numbers disable_jmx default_to_index.html rename_application fix_application_port_numbers setup_logging jakarta-tomcat-4.1.29/conf/tomcat-users.xml

configure update_config_files:	config_setup_sql_server default_to_index.html enable_internal_tools enable_smoke_tests fix_port_numbers setup_logging

edit_config config_setup_sql_server:	config-env/config/config.xml

edit_config:
	@echo Make sure to manually comment out DB2 section, and to uncomment SQL server section
	edit config-env/config/config.xml

disable_jmx:
	sed -i${if ${BACKUP_CONFIG_FILES},.bak} -e 's/\(param name="JMXEnabled" value="\)true/\1false/' config-env/config/config.xml

ifeq (${PROJECT_NAME}, ${APPLICATION_CODE})
update_config_files:	rename_application
endif

default_to_index.html:
	sed -i.bak -e 's/ClaimCenter.do/index.html/' config-env/data/WEB-INF/struts-config.xml
	sed -i.bak -e 's_\.\./ClaimCenter.do_pages/logins.xml_' config-env/config/pages/index.html

rename_application:
	-sed -i${if ${BACKUP_CONFIG_FILES},.bak} -e 's,/${APPLICATION_CODE},/${PROJECT_NAME},' \
		config-env/config/config.xml \
		config-env/config/plugins/webaddons/webaddons.properties \
		${CMD-TOOLS}/server.properties \
		${STUDIO}/bootstrap/studio.xml
	-sed -i${if ${BACKUP_CONFIG_FILES},.bak} -e 's,\(name="war.name" value="\)[^"]*,\1${PROJECT_NAME},' \
		config-env/bin/deploy.xml

fix_port_numbers:	fix_application_port_numbers

fix_application_port_numbers:
	@${TRACE} Fixing port numbers
	mv -f ${APPSRVR_CONFIG_FILE} ${APPSRVR_CONFIG_FILE}.bak
	awk '\
		NR==2 {print RS; sub(/port="[0-9]+"/,"port=\"${APPLICATION_PORT}\"")}; \
		{print}' \
		RS='on port ${APPLICATION_PORT_DEFAULT} -->' \
		ORS="" ${APPSRVR_CONFIG_FILE}.bak > ${APPSRVR_CONFIG_FILE}
	-sed -i${if ${BACKUP_CONFIG_FILES},.bak} -e ' \
		s/${APPLICATION_PORT_DEFAULT}/${APPLICATION_PORT}/g; \
		s/${APPSRVR_START_PORT_DEFAULT}/${APPSRVR_START_PORT}/g; \
		s/${APPSRVR_STOP_PORT_DEFAULT}/${APPSRVR_STOP_PORT}/g' \
			config-env/config/config.xml \
			config-env/config/plugins/webaddons/webaddons.properties \
			${CMD-TOOLS}/server.properties \
			${STUDIO}/bootstrap/studio.xml \
			${APPSRVR_CONFIG_FILE}

config_setup_sql_server:
	@${TRACE} Configuring for SQL Server
#	todo: what if username or password missing?
	sed -i${if ${BACKUP_CONFIG_FILES},.bak} -e ' \
		/sqlserver/{ \
		   s,\(:sqlserver://\)[^:]*,\1'${APPSRVR_HOST}',; \
		   s,\(;DatabaseName=\)[^;"]*,\1'${DATABASE_NAME}',; \
		   s,\(;User=\)[^;"]*,\1'${DATABASE_USER}',; \
		   /Password=/!s,"/>,;Password="/>,; \
		   s,\(;Password=\)[^;"]*,\1'${DATABASE_PASSWORD}',; \
		}' config-env/config/config.xml

enable_financials:
	@${TRACE} Enabling Financials
	@grep -q '<param name=.financials. value=.2./>' config-env/config/config.xml \
		&& echo Financials already enabled \
		|| sed -i${if ${BACKUP_CONFIG_FILES},.bak} -e '/<\/config>/i\
\  <!-- editable financials version -->\n\
   <param name="financials" value="2"/>\n' config-env/config/config.xml

enable_internal_tools:
	@${TRACE} Enabling Internal Tools
	@grep -q '<param name=.EnableInternalDebugTools. value=.true.' config-env/config/config.xml \
		&& echo Internal Tools already enabled \
		|| sed -i${if ${BACKUP_CONFIG_FILES},.bak} -e '/<\/config>/i\
\  <!-- This setting MUST be set false in production -->\n\
  <param name="EnableInternalDebugTools" value="true"/>\n' config-env/config/config.xml

enable_smoke_tests:
	@${TRACE} Enabling Smoke Tests
	@grep -q '<param name=.SmokeTestScriptDir. value=.true.' config-env/config/config.xml \
		&& echo Smoke Tests already enabled \
		|| sed -i${if ${BACKUP_CONFIG_FILES},.bak} -e '/<\/config>/i\
   <param name="SmokeTestScriptDir" value="${SMOKE_SCRIPT_DIR}"/>\n' config-env/config/config.xml

${APPSRVR_PATH}/conf/tomcat-users.xml:
	ruby -p -i${if ${BACKUP_CONFIG_FILES},.bak} -e ' \
		[%q!role rolename="manager"!, %q!role rolename="admin"!, %q!user username="su" password="tomcat" roles="admin,manager"!].each {|e| printf("  <%s/>\n", e)} if %r!<\/tomcat!' $@


# ================================================
# ======= Setup logging.properties

.PHONY:	setup_logging
LOG_DIR_M=$(shell cygpath -am ${LOG_DIR})
setup_logging:
	sed -ie 's,.File=[A-Za-z]:/.*/,.File=${LOG_DIR_M}/,' config-env/config/logging/logging.properties


# ================================================
# ======= Setup Database

dbinfo:
	awk '/DatabaseName/ {print $2}' RS=';' FS='=' config-env/config/config.xml

dbexists:
	${ISQL} ${DATABASE_NAME} -Q "SELECT COUNT(*) FROM sysusers" >/dev/null || echo .phony

dbpopulated:
	${ISQL} ${DATABASE_NAME} -Q "SELECT COUNT(*) FROM cc_user" >/dev/null || echo .phony

reset_passwords:
	${ISQL} ${DATABASE_NAME} -Q "UPDATE cc_credential SET Password = 'u6gO8+FQil44b/OHI9OxCnwdPTA=' WHERE (Password <> 'nologin')"

dbadmin:
	mmc.exe /s "C:\Program Files\Microsoft SQL Server\80\Tools\BINN\SQL Server Enterprise Manager.MSC" &

dblist:
	@${ISQL} master -Q "SELECT dbid, crdate, name, filename FROM sysdatabases" | \
	awk 'NR>2 { \
		split($$1, id_time, " +"); \
		split($$3, path, "." $$2); \
		dbid = id_time[1]; \
		crdate = sprintf("%4d-%3s-%02d %05s", id_time[4], id_time[2], id_time[3], id_time[5]); \
		name = $$2; \
		printf("%3d  %-40s  %s\n", dbid, name, crdate); \
		print "     " path[1] "\n"}' RS=' +\r\n +' FS='([- ]+\r\n\t)+' OFS='\t'


# ================================================
# ======= Build war File

HELP_DISPLAYED_COMMANDS += build
build:	${DEPLOYABLE_PROJECT_PKG}

PROJECT_CHANGES=$(shell ${FIND} config-env/config \
	-name 'import' -prune \
	-o -name 'locatedb' \
	-o -follow -newer ${DEPLOYABLE_PROJECT_PKG} ! -print -quit 2>/dev/null || echo phony)

clear_project_changes:	${PROJECT_CHANGES}
	touch -f ${DEPLOYABLE_PROJECT_PKG} $?

# force rebuild of package if any files or directories have changed
.PHONY:	$(if ${PROJECT_CHANGES},${DEPLOYABLE_PROJECT_PKG})

config-env/config/pages/ccindex.html_old:
	cp $(subst _old,,$@) $@

config-env/config/pages/ccindex.html:	config-env/config/pages/ccindex.html_old index.html
	cp -f index.html $@

config-env/config/pages/logins.xml:	logins.xml
config-env/config/pages/logins.xsl:	logins.xsl

config-env/config/pages/logins.xml config-env/config/pages/logins.xsl:
	cp -f $? $@

${DEPLOYABLE_PROJECT_PKG}:	config-env/config/logging/logging.properties
	-[ -e index.html ] && make config-env/config/pages/ccindex.html config-env/config/pages/logins.xml config-env/config/pages/logins.xsl
	@${TRACE} Building $@
ifdef TOMCAT
	cd config-env/bin; ant -buildfile deploy.xml build-appserver-war
# -Dwar.name=${PROJECT_NAME}
endif
ifdef WEBSPHERE
	cd config-env/bin; ant -buildfile deploy.xml build-websphere-appserver-ear
# -Dwar.name=${PROJECT_NAME} -Dear.name=${PROJECT_NAME}
endif


# ================================================
# ======= Deploy Package

.PHONY:	deploy

HELP_DISPLAYED_COMMANDS += deploy undeploy redeploy

deploy:	${DEPLOYED_PROJECT_PKG}

ifneq (${DEPLOYED_PROJECT_PKG},${DEPLOYABLE_PROJECT_PKG})
${DEPLOYED_PROJECT_PKG}:	${DEPLOYABLE_PROJECT_PKG}
	@make undeploy
	@${TRACE} Deploying new ${PROJECT_SUFFIX} File
ifdef DEPLOY_VIA_SOFT_LINK
ifdef CYGROOT
# Tomcat/Win32 can't support soft links, so we move the file and reverse the link
	sleep 3
	mv     ${DEPLOYABLE_PROJECT_PKG}               $(dir $@)
	ln -fs ${PROJECT_DIR}$@                        $(dir ${DEPLOYABLE_PROJECT_PKG})
else
	ln -fs ${PROJECT_DIR}${DEPLOYABLE_PROJECT_PKG} $(dir $@)
endif
else
	cp -f  ${DEPLOYABLE_PROJECT_PKG}               $(dir $@)
endif
endif

undeploy:	stop
	@if [ -e ${DEPLOYED_PROJECT_PKG} -o -d ${DEPLOYED_PROJECT_DIR} ]; then \
		${TRACE} Undeploying existing ${PROJECT_SUFFIX} File; \
		rm -rf ${DEPLOYED_PROJECT_DIR} \
		       ${DEPLOYED_PROJECT_PKG} \
		       compilation_log.html; \
	fi

redeploy:	undeploy
	make deploy


# ================================================
# ======= Update deployed package

.PHONY:	deploy

HELP_DISPLAYED_COMMANDS += update run-copy

update run-copy:	config-env
	@${TRACE} Updating changes for ${PROJECT_TITLE}; echo
	ant -buildfile config-env/bin/copy.xml -D${PROJECT_NAME}.dir=${DEPLOYED_PROJECT_DIR}
	@echo; ${MESSAGE} You may need to \'make bounce_process\' for the changes to take effect


# ================================================
# ======= Appsrvr Process

# ifneq (${DEPLOYED_PROJECT_DIR}, /)
# ${DEPLOYED_PROJECT_DIR}:	${DEPLOYED_PROJECT_PKG}
# 	make bounce_process
# endif

HELP_DISPLAYED_COMMANDS += start stop

# Can't use system_tools script because it doesn't properly set the return code
#APPSRVR_STARTED_TEST=${CMD-TOOLS}/system_tools.sh -password ${ADMIN_PASSWORD} -ping
APPSRVR_STARTED_TEST=wget --quiet --delete-after http://${APPSRVR_HOST}:${APPLICATION_PORT} 2>/dev/null

.PHONY:	start stop start_process start_${APPSRVR} stop_process stop_${APPSRVR} bounce_process bounce_${APPSRVR}

start_process start_${APPSRVR}:	${APPSRVR_PATH}
	@${TRACE} Starting ${APPSRVR}
	classpath=`pwd`/bootstrap.jar:${JAVA_HOME_U}/lib/tools.jar; \
	repeat=${APPSRVR_POLL_REPEAT}; ${APPSRVR_STARTED_TEST} \
		&& ${MESSAGE} ${APPSRVR} 'is already running!' \
		|| (${DEBUG_APPSRVR_ENV} ${START_APPSRVR_CMD}; \
			while ! (${APPSRVR_STARTED_TEST}); do \
				[ "$$repeat" -le 0 ] && echo Gave up waiting on ${APPSRVR} && exit 1; \
				repeat=`expr $$repeat - 1`; \
				echo '   ... waiting (Press CTRL-C to cancel)'; \
				sleep ${APPSRVR_POLL_INTERVAL}; \
			done)

stop_process stop_${APPSRVR}:
	@${TRACE} Stopping ${APPSRVR}
	@${APPSRVR_STARTED_TEST} \
		&& (${STOP_APPSRVR_CMD}) \
		|| ${MESSAGE} ${APPSRVR} 'is not running!'

bounce_process bounce_${APPSRVR}:	stop_process start_process


# ================================================
# ======= Appsrvr Daemon/Service

ifdef CYGROOT

DEBUG_DISPLAYED_VARS += JAVA_HOME_U JVM_DLL_W LOG_DIR_W CATALINA_HOME_W

CATALINA_HOME_W ?= $(shell cygpath -aw ${APPSRVR_PATH})

# If app is deployed, put it in deployment, otherwise keep it under this dir
#LOG_DIR_W       ?= $(shell cygpath -aw $(shell [ -d ${DEPLOYED_PROJECT_DIR} ] \
#														&& echo ${DEPLOYED_PROJECT_DIR}/${LOG_DIR} \
#														|| echo logs))
# INIT_LOG_DIR_W  ?= $(shell cygpath -aw logs)

LOG_DIR_W       ?= $(shell cygpath -aw ${LOG_DIR})

install_service:	just_install_service start

reinstall_service:	uninstall_service install_service

ifdef JAVA_HOME
JAVA_HOME_U     ?= $(shell cygpath -au ${JAVA_HOME})
JVM_DLL_W       ?= $(shell cygpath -aw ${JAVA_HOME_U}/jre/bin/server/jvm.dll)
endif

just_install_service:
# ifdef TOMCAT
ifdef JAVA_HOME_U
	-chgrp -R SYSTEM ${APPSRVR_PATH} ${LOG_DIR}
	-chmod -R g+rw   ${APPSRVR_PATH} ${LOG_DIR}
	cd ${APPSRVR_PATH}/bin; \
	classpath=`pwd`/bootstrap.jar:${JAVA_HOME_U}/lib/tools.jar; \
	./tomcat -install '${SERVICE_NAME}' '${JVM_DLL_W}' -Djava.class.path=`cygpath -apw $${classpath}` -Dcatalina.home='${CATALINA_HOME_W}' -Xms128m -Xmx256m $(if ${DEBUG_APPSRVR},${APPSRVR_DEBUG_FLAGS}) -Xrs -start org.apache.catalina.startup.BootstrapService -params start -stop org.apache.catalina.startup.BootstrapService -params stop -out '${LOG_DIR_W}\stdout.log' -err '${LOG_DIR_W}\stderr.log ${DEBUG_APPSRVR_ARGS}'
else
	@echo JAVA_HOME_NOT_DEFINED
	exit 1
endif
# 	@echo
# else
# 	$(error Target '$@' not yet implemented for ${APPSRVR}!)
# endif

uninstall_service:	stop_service
ifdef TOMCAT
	-cd ${APPSRVR_PATH}/bin; ./tomcat -uninstall '${SERVICE_NAME}'
else
	$(error Target '$@' not yet implemented for ${APPSRVR}!)
endif

clean:	uninstall_service

bounce_service:	stop_service pause_3_sec start_service

start_service:
	if [ -d ${LOG_DIR} ]; then \
		chgrp -R SYSTEM ${LOG_DIR}; \
		chmod -R g+rw ${LOG_DIR}; \
	fi
	net $(subst _service,,$@) '${SERVICE_NAME}'

stop_service:
	-net $(subst _service,,$@) '${SERVICE_NAME}'
endif

show_service:	show_service_name
	@echo
	show_service -h ${APPSRVR_HOST} ${SERVICE_NAME}
#	regtool list -pv '${APPSRVR_HOST}:/HKEY_LOCAL_MACHINE/system/CurrentControlSet/Services/${SERVICE_NAME}'

show_service_name:
	@echo SERVICE_NAME=${SERVICE_NAME}


# ================================================
# ======= Start/Stop Appsrvr

APPSRVR_IS_SERVICE ?= false

ifneq (${HOST}, sed)
APPSRVR_IS_SERVICE  = true
endif

ifeq  (${APPSRVR_IS_SERVICE}, true)
APPSRVR_SUFFIX      = service
else
APPSRVR_SUFFIX      = process
endif

DEBUG_DISPLAYED_VARS += APPSRVR_IS_SERVICE APPSRVR_SUFFIX

start:	start_${APPSRVR_SUFFIX}
stop:	stop_${APPSRVR_SUFFIX}
stopall:	stop_service stop_appsrvr

bounce:	bounce_${APPSRVR_SUFFIX}


# ================================================
# ======= Import Data, Import Rules

HELP_DISPLAYED_COMMANDS += import_data_small import_data_large import_rules

import_data:	import_data_large

import_data_small import_data_large import_rules:
	@${TRACE} Importing $(subst import_,,$@)
	cd ${CMD-TOOLS}/testdata_cc; ./$(subst _small,,$@).sh -server ${BASE_URL} -password ${ADMIN_PASSWORD}


# ================================================
# ======= Import System Tables

.import_system_tables:	${CMD-TOOLS}
#	$(shell ${FIND} config-env/config/import -type f -name '*.xml')
	make -s start
	cd ${CMD-TOOLS}; ./import_tools${SCRIPT_FILE_EXT} -password ${ADMIN_PASSWORD} $(addprefix ../,${FIND} config-env/config/import -type f -name '*.xml')
	make -s bounce


# ================================================
# ======= Compile the Pages

HELP_DISPLAYED_COMMANDS += compile
compile:	compilation_log.html
compilation_log.html:
	-make start
	@${TRACE} Compiling JSPs
	wget --post-data='doCompile=true' --output-document $@ ${BASE_URL}/CompileAll.do 2> /dev/null
	@grep -q 'ALL COMPILATIONS OK' $@ || (echo 'FAILED!'; ${BROWSER} $@; false)
	@${TRACE} All Compilations OK

show_docs:
	acrobat doc/pdf/${PROJECT_TITLE}_*.pdf &x


# ================================================
# ======= Deploy extra files

.PHONY:	update_logins

HELP_DISPLAYED_COMMANDS += ${DEPLOYED_PROJECT_DIR}/config.make ${DEPLOYED_PROJECT_DIR}/doc update_logins

# ${DEPLOYED_PROJECT_DIR}

${DEPLOYED_PROJECT_DIR}/config.make:	config.make
	${TRACE} Deploying config.make
	sed -e 's/$$/<br>/g' config.make > $@

${DEPLOYED_PROJECT_DIR}/doc:	${DEPLOYED_PROJECT_DIR}
	${TRACE} Deploying documentation
	mkdir $@
	cp -r ${HOME}/doc/guidewire/doc $@

${DEPLOYED_PROJECT_DIR}/index.html:	config-env/config/pages/ccindex.html
${DEPLOYED_PROJECT_DIR}/pages/logins.xsl:	config-env/config/pages/logins.xsl
${DEPLOYED_PROJECT_DIR}/pages/logins.xml:	config-env/config/pages/logins.xml

${DEPLOYED_PROJECT_DIR}/index.html ${DEPLOYED_PROJECT_DIR}/pages/logins.xsl ${DEPLOYED_PROJECT_DIR}/pages/logins.xml:
	cp $? $@

update_logins:	${DEPLOYED_PROJECT_DIR}/index.html ${DEPLOYED_PROJECT_DIR}/pages/logins.xsl ${DEPLOYED_PROJECT_DIR}/pages/logins.xml


# ================================================
# ======= Start the Application

.PHONY:	start${APPLICATION_CODE}

HELP_DISPLAYED_COMMANDS += start${APPLICATION_CODE}

ifndef RUNNING_REMOTELY
start${APPLICATION_CODE}:	start
endif

start${APPLICATION_CODE}:	juststart${APPLICATION_CODE}

juststart${APPLICATION_CODE}:
	@${TRACE} Starting web client
	$(if ${SSH_CLIENT},@echo )${BROWSER} ${BASE_URL}

show_url:
	@echo ${BASE_URL}


# ================================================
# ======= Reload Server Configuration

.PHONY:	reload_configuration

HELP_DISPLAYED_COMMANDS += reload_configuration

reload_configuration:	update_logins
	links -dump ${BASE_URL}/ReloadConfiguration.do


# ================================================
# ======= Run the Guidewire Studio

.PHONY:	start_studio

HELP_DISPLAYED_COMMANDS += start_studio

ifndef RUNNING_REMOTELY
start_studio:	start
endif

# To start studio remotely override the env var HOST, e.g.
#   HOST=ausable make start_studio
start_studio:	juststart_studio

juststart_studio:
	@${TRACE} Starting Guidewire Studio
	@${TRACE} Login as ${ADMIN_USER}/${ADMIN_PASSWORD}
	cd studio; nohup studio.sh ${BASE_URL} &


# ================================================
# ======== locate Databases

${LOCATE_DBS}:
	@${TRACE} Rebuilding $@
	cd $(dir $@); updatedb --changecwd=. --localpaths=. --output=$(notdir $@)

.PHONY:	$(shell for file in ${LOCATE_DBS}\; \
	do 	if [ -e $$file ]\; then \
			${FIND} `dirname $$file` ! -name locatedb -newer locatedb -print -quit | sed "s,..*,$$file,"\; \
		else \
			echo $$file\; \
		fi\; \
	done)

locatedbs:
	make ${LOCATE_DBS}


# ================================================
# ======= Reporting

.PHONY:	list_base_customHandlers list_customHandlers list_customHandlers_ext
HELP_DISPLAYED_COMMANDS += list_base_customHandlers list_customHandlers list_customHandlers_ext

# list_base_customHandlers:	$(shell ${FIND} config-env/config/elements -name '*.xml' ! -name '*_ext.xml')
# list_customHandlers:	$(shell ${FIND} config-env/config/elements -name '*.xml')
# list_customHandlers_ext:	$(shell ${FIND} config-env/config/elements -name '*_ext.xml')

list_base_customHandlers list_customHandlers list_ext_customHandlers:
	@${TRACE} I found these customeHandlers:
	@echo
	awk 'BEGIN {print}; \
		/customHandler="[^"]*"/ {\
			$$0 = $$1; \
			gsub("customHandler=\"",""); \
			gsub("\".*",""); \
			print $1; \
		}' RS=' ' config-env/config/elements/*.xml | sort -u


# ================================================
# ======= config.make

CLEANFILES += config.make
config.make config.new:
	@${TRACE} Regenerating $@
	rm -f $@
#	@temp_f=`mktemp $$TMP/$$$$.XXXXXX`
	make -s show_config 2>/dev/null > $@
#	mv -f $$temp_f $@
	@echo; echo
	@${WARN_MSG} \'$@\' has been generated with default
	@${WARN_MSG} values for ports, paths, etc. Update it to suit
	@${WARN_MSG} your installation and then rerun \'make\'.
	emacs -nw $@

update_config:	config.new
	mv config.make config.old
	mv config.new  config.make

SECTION=@printf '\n\# ================================================\n\# ======= %s\n\n'


HELP_DISPLAYED_COMMANDS += show_config

show_config:
	@echo '# -*- mode: Makefile -*-'
	@echo

	${SECTION} 'Makefile Environment'
	@echo '# Originally generated'
	@echo '#    on: $(shell date)'
	@echo '#   for: ${APPSRVR}'
	@echo -n '# under: '; uname

	@echo
	${SECTION} 'Project Parameters'
#		print "PROJECT_RELEASE     ?=", $$2;
	@awk '/release/ { \
		match($$2, /[0-9][.0-9]*$$/); \
		print "APPLICATION_CODE      ?=", substr($$2,0,RSTART-1); \
		print "PROJECT_VERSION       ?=", substr($$2,RSTART,RSTART+RLENGTH); \
		}' FS='=' OFS=' ' config-env/bin/registry.properties
	@echo 'PROJECT_RELEASE       ?= $${APPLICATION_CODE}-$${PROJECT_VERSION}'
	@echo 'CUSTOMER              ?= $(notdir $(shell pwd))'
	@echo
	@echo '# If you change this, be sure to update config-env/bin/deploy.xml'
	@echo 'PROJECT_NAME          ?= $${CUSTOMER}_$${PROJECT_RELEASE}'
	@echo
	@echo 'PROJECT_TITLE         ?= ClaimCenter'

	@echo 'ADMIN_USER            ?= su'
	@echo 'ADMIN_PASSWORD        ?= $${APPLICATION_CODE}'
	@echo 'BACKUP_CONFIG_FILES   ?= true'
	@echo 'SMOKE_SCRIPT_DIR      ?= $${PROJECT_DIR}$${DEPLOYED_PROJECT_DIR}/config/smoke-tests'

	@echo
	${SECTION} 'Database Parameters'
	@echo 'DATABASE_SERVER       ?= localhost'
	@echo 'DATABASE_NAME         ?= $${PROJECT_NAME}'
	@echo 'DATABASE_USER         ?= sa'
	@echo 'DATABASE_PASSWORD     ?= 123'

	@echo
	${SECTION} 'Application Server Parameters'
	@echo 'APPLICATION_PORT      ?= ${APPLICATION_PORT}'
	@echo 'BASE_URL              ?= http://$${APPSRVR_HOST}:$${APPLICATION_PORT}/$${PROJECT_NAME}'
	@echo 'APPSRVR_POLL_INTERVAL ?= ${APPSRVR_POLLING_DEFAULT}'
	@echo 'APPSRVR               ?= ${APPSRVR}'
	@echo 'APPSRVR_IS_SERVICE    ?= false'


ifdef CYGROOT
	@echo
	${SECTION} 'Cygwin-specific Parameters'
	@echo 'SERVICE_NAME          ?= $${APPLICATION_CODE} $${PROJECT_NAME} $${PROJECT_VERSION} on port $${APPLICATION_PORT}'
	@echo 'START                 ?= cmd /C'
endif

	@echo
	${SECTION} '${APPSRVR}-specific Parameters'
	@echo -n 'APPSRVR_PATH          ?= '
	@echo $(shell ${FIND} -maxdepth 1 -type d -iname "*${APPSRVR}*" -printf "%f")
	@echo 'APPSRVR_CONFIG_FILE   ?= $${APPSRVR_PATH}/conf/server.xml'
	@echo 'APPSRVR_START_PORT    ?= $$(shell expr $${APPLICATION_PORT} + 1)'
	@echo 'APPSRVR_STOP_PORT     ?= $$(shell expr $${APPLICATION_PORT} + 2)'
ifdef TOMCAT
	@echo 'JPDA_ADDRESS          ?= $$(shell expr $${APPLICATION_PORT} + 3)'
	@echo
	@echo 'DEPLOYMENT_DIR        ?= $${APPSRVR_PATH}/webapps'
	@echo
	@echo 'PROJECT_SUFFIX        ?= .war'
	@echo
ifdef CYGROOT
	@echo '# end with true command to mask -1 return code of batch file'
endif
	@echo 'START_APPSRVR_CMD     ?= cd $${APPSRVR_PATH}/bin; $${START} catalina.$(if ${CYGROOT},bat start; true,sh)'
	@echo 'STOP_APPSRVR_CMD      ?= $$(subst startup,shutdown,$${START_APPSRVR_CMD})'
else
ifdef WEBSPHERE
	@echo 'DEPLOYMENT_DIR        ?= $${APPSRVR_PATH}/'
	@echo
	@echo 'PROJECT_SUFFIX        ?= .ear'
	@echo
	@echo 'START_APPSRVR_CMD     ?= cd $${APPSRVR_PATH}/bin; '
	@echo 'STOP_APPSRVR_CMD      ?= $$(subst startup,shutdown,$${START_APPSRVR_CMD})'
endif
endif
	@echo 'LOG_DIR               ?= $${PROJECT_DIR}$${DEPLOYED_PROJECT_DIR}/config/logging'

	@echo
	${SECTION} 'Tuning Parameters'
	@echo '# Any nonempty string means deploy using soft (symbolic) links (vs. copying)'
	@echo 'DEPLOY_VIA_SOFT_LINK  ?= true'
