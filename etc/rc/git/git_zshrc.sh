#@IgnoreInspection BashAddShebang
# To install source this file from your .zshrc file

# see documentation at http://linux.die.net/man/1/zshexpn
# A: finds the absolute path, even if this is symlinked
# h: equivalent to dirname
export __GIT_PROMPT_DIR=${0:A:h}

export GIT_PROMPT_EXECUTABLE=${GIT_PROMPT_EXECUTABLE:-"python"}
export GIT_BRANCH

# Allow for functions in the prompt.
setopt PROMPT_SUBST

autoload -Uz add-zsh-hook

add-zsh-hook chpwd   update_git_vars_chpwd
add-zsh-hook precmd  updateGitVars
add-zsh-hook preexec update_git_vars_preexec

## Function definitions
function update_git_vars_preexec() {
    case "$2" in
        git*|hub*|gh*|stg*)
            __EXECUTED_GIT_COMMAND=1
            ;;
    esac
}

function update_git_vars_precmd() {
    if [ -n "$__EXECUTED_GIT_COMMAND" ] || [ ! -n "$ZSH_THEME_GIT_PROMPT_CACHE" ]; then
        update_current_git_vars
        unset __EXECUTED_GIT_COMMAND
    fi
}

function update_git_vars_chpwd() {
    update_current_git_vars
}

function update_current_git_vars() {
    unset __CURRENT_GIT_STATUS

    if [[ "$GIT_PROMPT_EXECUTABLE" == "python" ]]; then
        local gitstatus="$__GIT_PROMPT_DIR/gitstatus.py"
        _GIT_STATUS=`python ${gitstatus} 2>/dev/null`
    fi
    if [[ "$GIT_PROMPT_EXECUTABLE" == "haskell" ]]; then
        _GIT_STATUS=`git status --porcelain --branch &> /dev/null | $__GIT_PROMPT_DIR/src/.bin/gitstatus`
    fi
    __CURRENT_GIT_STATUS=("${(@s: :)_GIT_STATUS}")
	 GIT_BRANCH=$__CURRENT_GIT_STATUS[1]
	 GIT_AHEAD=$__CURRENT_GIT_STATUS[2]
	 GIT_BEHIND=$__CURRENT_GIT_STATUS[3]
	 GIT_STAGED=$__CURRENT_GIT_STATUS[4]
	 GIT_CONFLICTS=$__CURRENT_GIT_STATUS[5]
	 GIT_CHANGED=$__CURRENT_GIT_STATUS[6]
	 GIT_UNTRACKED=$__CURRENT_GIT_STATUS[7]
}


git_super_status() {
	update_git_vars_precmd

    if [ -n "$__CURRENT_GIT_STATUS" ]; then
        STATUS="$ZSH_THEME_GIT_PROMPT_PREFIX$ZSH_THEME_GIT_PROMPT_BRANCH$GIT_BRANCH%{${reset_color}%}"
        if [ "$GIT_BEHIND" -ne "0" ]; then
          STATUS="$STATUS$ZSH_THEME_GIT_PROMPT_BEHIND$GIT_BEHIND%{${reset_color}%}"
        fi
        if [ "$GIT_AHEAD" -ne "0" ]; then
          STATUS="$STATUS$ZSH_THEME_GIT_PROMPT_AHEAD$GIT_AHEAD%{${reset_color}%}"
        fi
        STATUS="$STATUS$ZSH_THEME_GIT_PROMPT_SEPARATOR"
        if [ "$GIT_STAGED" -ne "0" ]; then
          STATUS="$STATUS$ZSH_THEME_GIT_PROMPT_STAGED$GIT_STAGED%{${reset_color}%}"
        fi
        if [ "$GIT_CONFLICTS" -ne "0" ]; then
          STATUS="$STATUS$ZSH_THEME_GIT_PROMPT_CONFLICTS$GIT_CONFLICTS%{${reset_color}%}"
        fi
        if [ "$GIT_CHANGED" -ne "0" ]; then
          STATUS="$STATUS$ZSH_THEME_GIT_PROMPT_CHANGED$GIT_CHANGED%{${reset_color}%}"
        fi
        if [ "$GIT_UNTRACKED" -ne "0" ]; then
          STATUS="$STATUS$ZSH_THEME_GIT_PROMPT_UNTRACKED%{${reset_color}%}"
        fi
        if [ "$GIT_CHANGED" -eq "0" ] && [ "$GIT_CONFLICTS" -eq "0" ] && [ "$GIT_STAGED" -eq "0" ] && [ "$GIT_UNTRACKED" -eq "0" ]; then
          STATUS="$STATUS$ZSH_THEME_GIT_PROMPT_CLEAN"
        fi
        STATUS="$STATUS%{${reset_color}%}$ZSH_THEME_GIT_PROMPT_SUFFIX"
        echo "$STATUS"
	 fi
}

# Default values for the appearance of the prompt. Configure at will.
ZSH_THEME_GIT_PROMPT_AHEAD="%{↑%G%}"
ZSH_THEME_GIT_PROMPT_BEHIND="%{↓%G%}"
ZSH_THEME_GIT_PROMPT_BRANCH="%{$fg_bold[magenta]%}"
ZSH_THEME_GIT_PROMPT_CHANGED="%{$fg[blue]%}%{✚%G%}"
ZSH_THEME_GIT_PROMPT_CLEAN="%{$fg_bold[green]%}%{✔%G%}"
ZSH_THEME_GIT_PROMPT_CONFLICTS="%{$fg[red]%}%{✖%G%}"
ZSH_THEME_GIT_PROMPT_PREFIX="("
ZSH_THEME_GIT_PROMPT_SEPARATOR="|"
ZSH_THEME_GIT_PROMPT_STAGED="%{$fg[red]%}%{●%G%}"
ZSH_THEME_GIT_PROMPT_SUFFIX=")"
ZSH_THEME_GIT_PROMPT_UNTRACKED="%{…%G%}"

# ZSH_THEME_GIT_PROMPT_BEHIND="%{$fg_bold[magenta]%}%{↓%G%}"
ZSH_THEME_GIT_PROMPT_BRANCH="%{$fg[magenta]%}"
ZSH_THEME_GIT_PROMPT_BRANCH="%{\e[38;5;105m%}"
ZSH_THEME_GIT_PROMPT_CHANGED="%{\e[94m%}%{+%G%}"
ZSH_THEME_GIT_PROMPT_SEPARATOR="%{$fg_bold[grey]%}|"
# ZSH_THEME_GIT_PROMPT_STAGED="%{\e[38;5;215m%}%{∘%G%}"
# ZSH_THEME_GIT_PROMPT_AHEAD="%{$fg_bold[magenta]%}%{↑%G%}"

updateGitVars () {
    GIT_SUPER_STATUS=`git_super_status`
    if [ -z "$GIT_SUPER_STATUS" -o "$GIT_BRANCH" = ":" ]; then
        unset GIT_STATUS
    else
        GIT_STATUS="$GIT_SUPER_STATUS  ${fg_bold[grey]}`git config branch.$GIT_BRANCH.description`${reset_color}
"
    fi
    export GIT_REPO=`git config --get remote.origin.url | sed 's_.*/\(.*\)\.git_\1_g'`
}

add-zsh-hook precmd updateGitVars
update_current_git_vars