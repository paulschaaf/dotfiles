#!/usr/bin/env zsh

# source this at the start of your script

# if $trace is set log everything
(( $#trace )) && set -x

# if $zprof is set run the profiler
if (( $#zprof )); then
    zmodload zsh/zprof
    echo Running zprof from ${(%):-%N}
    unset zprof
fi
