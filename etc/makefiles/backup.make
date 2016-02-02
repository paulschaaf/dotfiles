# Makefile

include ~/lib/debugging.Makefile

BACKUP_DIR=/home/pschaaf/local/sync/backup/${HOSTNAME}/
ARCHIVE=7z a -r
EXTENSION=.7z

ifndef HOSTNAME
  HOSTNAME=$(shell hostname -s)
endif

# am I root?
ifeq (0,$(shell id -u))
  ROOT=true
  ALL=etc
else
  ifdef CYGWIN
    ALL=etc
  else
    ALL=kde
  endif
  ALL+=mail.7z ssh.7z svn-repository.7z
endif

PSCHAAF_HOME=/home/pschaaf

ifeq ($(shell pwd),${PSCHAAF_HOME})
  USER_HOME=./
else
  USER_HOME=${PSCHAAF_HOME}/
endif


DONE=&& echo ' done' || echo 'ERROR: '${ARCHIVE}
MSG_SIMPLE_REFRESHING=@if [ -e $@ ]; then echo -n \\\# Refreshing; else echo -n \\\# Creating; fi; echo -n ' $(notdir $@) ... '

MSG_REFRESHING=@if [ -e $@ ]; then echo \\\# Refreshing $(notdir $@) due to changes in; echo $^; else echo -n \\\# Creating; fi; echo -n ' $(notdir $@) ... '; echo


.SUFFIXES:
.PHONY:	${ALL} phony -printf refresh_target create_target

all:	${ALL}
list:
	@echo ${ALL}

DEBUG_DISPLAYED_VARS+=ALL HOSTNAME ROOT UNAME USER_HOME

# ================================================
# ======= SVN

SVNROOT_DIR=${HOME}/local/sync/svn-repository
#SVN_UPDATED=$(shell find ${SVNROOT_DIR} -follow -mindepth 1 ! -name '*:*' -newer ${BACKUP_DIR}svn-repository${EXTENSION} -print refresh_target 2> /dev/null || echo create_target)
DEBUG_DISPLAYED_VARS+=SVNROOT_DIR SVN_UPDATED

svn-repository${EXTENSION}:	${SVNROOT_DIR}
	${MSG_REFRESHING}
	${ARCHIVE} $@ $(shell cygpath -am $?) ${DONE}


# ================================================
# ======= etc

ETC_UPDATED=$(shell find /etc -follow -mindepth 1 -newer ${BACKUP_DIR}etc${EXTENSION} -print 2> /dev/null || echo create_target)
DEBUG_DISPLAYED_VARS+=ETC_UPDATED

etc:	${BACKUP_DIR}etc${EXTENSION}
${BACKUP_DIR}etc${EXTENSION}:	${ETC_UPDATED}
	${MSG_REFRESHING}
	${ARCHIVE} -f $@ /etc ${DONE}


# ================================================
# ======= KDE

# # don't follow symlinks here, they're just to cache
# KDE_UPDATED=$(shell find ${USER_HOME}.kde -mindepth 1 ! -name '*:*' -newer ${BACKUP_DIR}kde${EXTENSION} -print refresh_target 2> /dev/null || echo create_target)
# DEBUG_DISPLAYED_VARS+=KDE_UPDATED

# kde:	${BACKUP_DIR}kde${EXTENSION}
# ${BACKUP_DIR}kde${EXTENSION}:	${KDE_UPDATED}
# 	${MSG_SIMPLE_REFRESHING}
# 	${ARCHIVE} -C ${USER_HOME} -f $@ .kde ${DONE}


# ================================================
# ======= mail

MAIL_UPDATED=$(shell find ${USER_HOME}.mail -follow -mindepth 1 ! \( -name '*:*' -o -name 'Junk' -o -name 'Trash' \) -newer ${BACKUP_DIR}mail${EXTENSION} -print refresh_target 2> /dev/null || echo create_target)
DEBUG_DISPLAYED_VARS+=MAIL_UPDATED

mail:	${BACKUP_DIR}mail${EXTENSION}
${BACKUP_DIR}mail${EXTENSION}:	${MAIL_UPDATED}
	${MSG_SIMPLE_REFRESHING}
	${ARCHIVE} -C ${USER_HOME} -f $@ .mail ${DONE}


# ================================================
# ======= Palm

# PALM_PARENT_DIR=${USER_HOME}
# PALM_UPDATED=$(shell find ${PALM_PARENT_DIR} -follow -mindepth 1 ! -name '*:*' -newer ${BACKUP_DIR}palm${EXTENSION} -print refresh_target 2> /dev/null || echo create_target)
# DEBUG_DISPLAYED_VARS+=PALM_UPDATED

# palm:	${BACKUP_DIR}palm${EXTENSION}
# ${BACKUP_DIR}palm${EXTENSION}:	${PALM_UPDATED}
# 	${MSG_REFRESHING}
# 	${ARCHIVE} -C ${PALM_PARENT_DIR} -f $@ palm ${DONE}


# ================================================
# ======= ssh

SSH_PREREQS=.ssh/authorized_keys .ssh/config .ssh/environment .ssh/known_hosts
DEBUG_DISPLAYED_VARS+=SSH_PREREQS

ssh:	${BACKUP_DIR}ssh${EXTENSION}
${BACKUP_DIR}ssh${EXTENSION}:	${SSH_PREREQS}
	${MSG_REFRESHING}
	${ARCHIVE} -C ${USER_HOME} -f $@ $^ ${DONE}


# ================================================
# ======= Thunderbird

# THUNDERBIRD_UPDATED=$(shell find ${USER_HOME}.thunderbird -follow -mindepth 1 ! -name '*:*' -newer ${BACKUP_DIR}thunderbird${EXTENSION} -print refresh_target 2> /dev/null || echo create_target)
# DEBUG_DISPLAYED_VARS+=THUNDERBIRD_UPDATED

# thunderbird:	${BACKUP_DIR}thunderbird${EXTENSION}
# ${BACKUP_DIR}thunderbird${EXTENSION}:	${THUNDERBIRD_UPDATED}
# 	${MSG_SIMPLE_REFRESHING}
# 	${ARCHIVE} -C ${USER_HOME} -f $@ .thunderbird ${DONE}

