#!/bin/bash
input=$(cat)
prompt=$(jq -r '.prompt // ""' <<<"$input" | head -c 40)
dir=$(basename "$PWD")
seq=$(printf '\033]0;%s: %s\007' "$dir" "$prompt")
jq -nc --arg seq "$seq" '{terminalSequence: $seq}'