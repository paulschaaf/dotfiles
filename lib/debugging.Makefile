# ================================================
# ======= Debugging

DEBUG_DISPLAYED_VARS ?=

# Make sure 'all' is the first target, otherwise it might treat
# debug as the default target
all:

.PHONY:	debug _dump_variables

debug:	_dump_variables

VARS_FROM = $(shell sed -ne 's/\#.*//g; s/=.*$$//gp' ${1} ${2} ${3} ${4} ${5} ${6} ${7} ${8} ${9})


# Add variable names to this variable to have them displayed, e.g.
# DEBUG_DISPLAYED_VARS += ANT CLASSPATH

_dump_variables:
	@echo
	@echo \# DEBUG: Printing every variable named in DEBUG_DISPLAYED_VARS
	@$(foreach varname,$(sort ${DEBUG_DISPLAYED_VARS}),\
		printf "%-22s := %s\n" ${varname} '${${varname}}';)
