#!/bin/sh

app=`basename $0`
# echo; echo app is ${app}

synonyms='acrobat acroread excel gimp outlook ppt textpad vdiff word'

show_usage () {
    if [ -z "$meta_usage" ]; then
        echo "Usage: $app <switches> FILE..."
        echo Open the file\(s\) using $app.
        echo '  -n, --dry-run 'Display the full command that would be invoked.
        echo '  --synonyms    'Display alternate names for this application.
    else
        echo Create a link to $app named after a supported application \(shown below\).
        echo Executing that link will launch the application it is named after with any
        echo supplied arguments.
        echo
        echo In Windows each application is stored in its own directory. Rather than add
        echo each directory to your PATH, simply register the executable in $app under
        echo a convenient name, and create a link using that name in a directory that is
        echo already in your PATH, like /usr/local/bin.
        echo
        echo Supported Applications:
    fi
}

show_synonyms () {
    for synonym in $synonyms; do
      [ "$synonym" != "$app" ] && echo '  '$synonym
    done | sort -u
}

unset echo files switches

PROG_FILES='/c/PROGRA~1/'
PROG_FILES_x86='/c/PROGRA~2/'
OFFICE=${PROG_FILES_x86}'/Microsoft Office/Office12/'
case ${app} in
    acrobat|acroread)
        PROG_FILES+='Adobe/Reader*/Reader/AcroRd32.exe'
        ;;

    excel)
        PROG_FILES=${OFFICE}EXCEL.EXE
        ;;

    gimp)
        PROG_FILES+="GIMP-2.2/bin/gimp-2.2.exe"
        ;;

    launch.sh)
        case ${1} in
            -h|--help)
                meta_usage=true
                show_usage
                ;;
            *)
                ;;
        esac
        show_synonyms
        exit
        ;;

    outlook)
        PROG_FILES=${OFFICE}OUTLOOK.EXE
        ;;

    ppt)
        PROG_FILES=${OFFICE}POWERPNT.EXE
        ;;

    textpad)
        PROG_FILES+="TextPad 5/TextPad.exe"
        ;;

    vdiff)
        PROG_FILES="${PROG_FILES}/Araxis/ARAXIS~1/AraxisP4Diff.exe"
        ;;

    word)
        PROG_FILES=${OFFICE}WINWORD.EXE
        ;;

    *)
        echo "ERROR: I don't know how to behave when I'm named '$0'"
        exit 1
        ;;
esac

while [ -n "$1" ]; do
    case ${1} in
        --)
            shift
            break
            ;;
        -help)
            # Needed for zsh auto-complete
            echo \'$PROG_FILES\'
            exit
            ;;
        --debug)
            shift
            export debug=1
            set -x
            ;;
        -h|--help)
            show_usage
            exit
            ;;
        -n|--dry-run)
            shift
            echo='echo '
            ;;
        --synonyms)
            show_synonyms
            exit
            ;;
        -*)
            echo "ERROR: unrecognized switch '$1'"
            echo
            show_usage
            exit 1
            ;;
        *)
            break
            ;;
    esac
done

case `uname -o` in
    Cygwin)
        # PROG_FILES=$(echo $PROG_FILES | sed 's_\([ !]\)_\\\1_g')
        PROG_FILES=$(cygpath -asm "$PROG_FILES" | cygpath -uf -)
        files=$(cygpath  -asm "$@")
        ;;

    *)
        files=$(echo $@ | sed "s,^$HOME,w:,g;"' s,/,\\\\,g')
        PROG_FILES='wine --check --cx-app '${PROG_FILES}
        ;;
esac

[ -n "$echo" ] && PROG_FILES="echo '"${PROG_FILES}"'"

cmd=${PROG_FILES}${switches}\ ${files}

#exec `echo $cmd`

if [ -n "${echo}" ]; then
    echo ${cmd}
else
    ${cmd} &
fi
