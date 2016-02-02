#!/bin/bash

#set -x

echo 0=$0
echo 1=$1
echo 2=$2

PATH=`/usr/bin/cygpath -aup $CYGPATH`:$PATH
export PATH

if [ "$1" != "startup" ]; then
    if [ "$1" = "restart" -o "$1" = "stop" -o "$1" = "kill" ]; then
        echo Killing existing agent
        pkill ssh-agent
        [ "$1" = "stop" -o "$1" = "kill" ] && exit
        shift
    fi

    /bin/pgrep ssh-agent > /dev/null
    if [ "$?" -eq "0" ]; then
        echo Use \"start-ssh-agent.sh restart\" if you want to kill the existing daemon.
        echo Press Enter to quit...; read
        exit
    fi
fi

rm -rf /tmp/ssh-*
#ssh-agent
eval `ssh-agent`
#eval `/C/PROGRA~2/Git/bin/ssh-agent`

set +x
#echo $SSH_AGENT_PID; echo $SSH_AUTH_SOCK

UNAME=`uname`
unamePrefix=${UNAME//_*/}_

for var in SSH_AGENT_PID SSH_AUTH_SOCK; do
    export ${var}
    eval value=\$${var}

    # set values in global environment for Windows user
    setx ${unamePrefix}${var} ${value}
    echo setx ${unamePrefix}${var} ${value}
done

# echo $SSH_ASKPASS=

if [ -e $HOME/.ssh/ssh-open ]; then
    echo adding ssh-open
    cat $HOME/.ssh/ssh-open | xargs ssh-add
fi

# echo; echo "======= CYGWIN"
# /c/cygwin/bin/ssh-add
ssh-add
# echo; echo "======= GIT BASH"
# /c/PROGRA~2/Git/bin/bash.exe -c '/c/PROGRA~2/Git/bin/ssh-add'

echo Press Enter to continue...; read
