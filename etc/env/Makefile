ifdef CYGROOT
# Ignore warnings about insecure directories under Cygwin
# RUBY_SWITCHES=-W$${RUBY_WARN_LEVEL:=0}
endif

HOST  ?= $(shell hostname)
UNAME ?= $(shell uname)

ALL_HOSTS=ausable menominee styx sed ftp service01 

LEGALIZE_ARGS=--legalize='generic laptop ruby styx mail copernicus satori'

CMD_START=rm -f $@; echo \\\# === Making $@
CMD=ALL_HOSTS=${ALL_HOSTS} HOST=${HOST} ruby ${RUBY_SWITCHES} ${RUBYLIB_ARGS} port.rb ${LEGALIZE_ARGS} --host=$(basename $@)
CMD_END= > $@

# ==================== Common Rules

.SUFFIXES:

.PHONY:

all:	localhost ${AUTOMATIC_HOSTS}

localhost:	${HOST}

ALL_SHELLS=${HOST}.screen ${HOST}.sh ${HOST}.tcsh ${HOST}.zsh
${ALL_SHELLS}:	port.rb

clean:
	rm -rf ${ALL_SHELLS}

rebuild:	clean all

.screenrc-full.rb:	servers.rb keyboard.rb screenrc.rb
.shellrc-full.rb:	servers.rb keyboard.rb shellrc.rb

.screenrc-full.rb .shellrc-full.rb:
	temp=`mktemp $$$$.XXXXXX`; ruby -n -e 'puts gsub(/^(# )-(====== .*)/, %q{comment \1==\2}) unless /^#([^ ]| [^=-])/ =~ $$_' $^ >> $$temp; mv -f $$temp $@


# ==================== Hosts

${HOST}:	${HOST}.screen ${HOST}.zsh

${HOST}.sh ${HOST}.tcsh ${HOST}.zsh:	.shellrc-full.rb
	${CMD_START}
	${CMD} --shell=$(suffix $@) $? ${CMD_END}

${HOST}.screen:	.screenrc-full.rb
	${CMD_START}
	${CMD} --shell=$(suffix $@) $? ${CMD_END}
