#!/usr/bin/env python3
"""gwb.py — Guidewire Build wrapper (Python port of bin/gwb)"""

# Requires Python 3.9+

import os
import re
import shutil
import subprocess
import sys
import time
from dataclasses import dataclass
from datetime import datetime
from pathlib import Path

# ── Terminal colors ───────────────────────────────────────────────────────────
run_color  = os.environ.get('run_color',  '48;5;21')
pass_color = os.environ.get('pass_color', '48;5;34')
fail_color = os.environ.get('fail_color', '48;5;196')
warn_color = os.environ.get('warn_color', '48;5;228')
note_color = os.environ.get('note_color', '97;46')

PKILL     = 'pkill -KILL -f --'
DB_BACKUP = 'h2db'
FAIL_ICON = '/System/Library/CoreServices/CoreTypes.bundle/Contents/Resources/AlertStopIcon.icns'

APD_DIRS = [
    'app-cc/cc-apd-genlob-content/build',
    'app-cc/cc-apd-genlob-content/config',
    'app-cc/cc-apd-genlob-content/generated',
    'app-cc/cc-apd-genlob-content/generated_classes',
    'app-cc/cc-apd-genlob-content/gsrc',
    'app-cc/cc-apd-genlob-content/gtest/gw/smoketest/apd',
    'app-cc/cc-apd-genlob-content/src',
    'app-cc/cc-test-apd/build',
    'app-cc/cc-test-apd/config',
    'app-cc/cc-test-apd/generated',
    'app-cc/cc-test-apd/generated_classes',
    'app-cc/cc-test-apd/gsrc',
    'app-cc/cc-test-apd/gtest/gw',
]

EXPECTED_PLUGINS = [
    'GitHub Copilot',
    'GitLink',
    'GitToolBox',
    'Grep Console',
    'Jump to Line',
    'Launch URL from String',
    'PlantUML Parser',
    'plantuml4idea',
    'Run Configuration as Action',
    'String Manipulation',
]

PUML_HEADER = """\
@startuml
!pragma useIntermediatePackages false
'scale 1800 width
'scale 4000 height
hide empty members
'left to right direction
'set separator none
skinparam class {
  BackgroundColor<<gosu>> lightgreen
  BackgroundColor<<internal>> lightblue
  AttributeFontColor<<legacy>> Gray
  StereotypeFontColor<<legacy>> Gray
}
package entity #wheat {}

@enduml
"""


# ── Task ──────────────────────────────────────────────────────────────────────

@dataclass
class Task:
    name: str
    command: str        = ''
    spawn: bool         = False   # run with nohup in background, don't wait
    eval_it: bool       = False   # eval in current shell process (e.g. cd)
    clipboard: bool     = False   # pipe output to pbcopy
    abort_on_fail: bool = False   # sys.exit immediately on non-zero exit code
    ignore_nice: bool   = False   # bypass the global --nice flag for this task
    notify: bool        = True    # send OS notification on success
    pass_icon: str      = ''
    fail_icon: str      = ''


# ── Env setup ─────────────────────────────────────────────────────────────────

def source_gwsetenv():
    """Source $HOME/bin/gwSetEnv and merge its exports into os.environ."""
    script = Path.home() / 'bin' / 'gwSetEnv'
    result = subprocess.run(
        ['zsh', '-c', f'source {script} && env -0'],
        capture_output=True, text=True,
    )
    if result.returncode != 0 or not result.stdout:
        print('\033[1;48;5;196mCould not source gwSetEnv!\033[0m', file=sys.stderr)
        sys.exit(1)
    for item in result.stdout.split('\0'):
        if '=' in item:
            key, _, value = item.partition('=')
            os.environ[key] = value
    if not os.environ.get('GW_PL_VERSION'):
        print('\033[1;48;5;196mCould not determine Guidewire platform version!\033[0m',
              file=sys.stderr)
        sys.exit(1)


# ── Helpers ───────────────────────────────────────────────────────────────────

def log_section(color, status, task_desc):
    now = datetime.now().strftime('%H:%M')
    status_str = f'{now} - {status} '
    cols = shutil.get_terminal_size((80, 24)).columns
    print(f'\n\033[97;{color}m{status_str}\033[0;38;5;243m')
    print(f'\033[0;97;{color}m{task_desc:<{cols}}\033[0m')


def settitle(title=''):
    sys.stdout.write(f'\033]0;{title}\007')
    sys.stdout.flush()


def notify(subtitle, message, app_icon=None):
    if not shutil.which('terminal-notifier'):
        return
    gw_title = os.environ.get('GW_TITLE', 'GW')
    cmd = ['terminal-notifier', '-group', os.getcwd(),
           '-title', gw_title, '-subtitle', subtitle, '-message', message]
    if app_icon:
        cmd += ['-appIcon', app_icon]
    subprocess.Popen(cmd, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)


def grep_proc_str(pattern):
    return f"pgrep -afl -- '{pattern}' | grep -v pgrep"


def debug_print(debug, *args):
    if debug:
        print(f'\033[35m{" ".join(str(a) for a in args)}\033[0m')


# ── Task runner ───────────────────────────────────────────────────────────────

def run_task(task: Task, *, dry_run=False, delay=0, nice=False, debug=False):
    """Execute a task, bracketing it with START/DONE/FAIL log lines."""
    gw_title = os.environ.get('GW_TITLE', 'GW')
    gw_start = os.environ.get('GW_START_SCRIPT', './gwb')

    task_summary = re.sub(r'\s*\|\s*sed.*$', '', task.command)

    debug_print(debug, 'alpha')
    debug_print(debug, 'beta')

    if delay > 0:
        print(f'\nDelay for {delay}')
        time.sleep(delay)

    title_suffix = task_summary.removeprefix(gw_start).strip()
    settitle(f'{gw_title} {title_suffix}')
    log_section(run_color, f"START '{task.name}'", task_summary)

    exit_code = 0
    start_time = time.time()

    if not dry_run:
        cmd = task.command
        if not task.ignore_nice and nice:
            cmd = f'nice {cmd}'
        if task.clipboard:
            cmd = f'{cmd} | pbcopy'

        if task.eval_it:
            # eval_it tasks are directory changes — perform them in-process so
            # all subsequent tasks run in the new directory.
            target = os.path.expanduser(cmd.removeprefix('cd ').strip())
            try:
                os.chdir(target)
            except OSError as e:
                print(e, file=sys.stderr)
                exit_code = 1
        elif task.spawn:
            subprocess.Popen(
                f'nohup {cmd}',
                shell=True, executable='/bin/zsh',
                stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL,
            )
        else:
            # Java commands: display each -option on its own indented line.
            # Append exit ${pipestatus[1]} so we capture the task's exit code, not sed's.
            piped = (
                f"{cmd} | sed '/java / s/ -/\\n   -/g'; "
                f"exit ${{pipestatus[1]}}"
            )
            exit_code = subprocess.run(piped, shell=True, executable='/bin/zsh').returncode

            if task.clipboard:
                print(f'\n\033[{note_color}mAdded the following to the clipboard\033[0m')
                subprocess.run(['pbpaste'])
                print()

    elapsed     = time.time() - start_time
    elapsed_str = time.strftime('in %H:%M:%S', time.gmtime(elapsed))

    if exit_code == 0:
        log_section(pass_color, f"DONE '{task.name}' {elapsed_str}", task_summary)
        if task.notify:
            notify(f'Completed: "{task.name}"', task_summary)
    else:
        exit_str = f'---with exit code {exit_code}' if exit_code > 1 else ''
        log_section(fail_color, f"FAIL '{task.name}' {elapsed_str}{exit_str}", task_summary)
        notify(f'Failed({exit_code}): "{task.name}"', task_summary,
               app_icon=task.fail_icon or None)
        if task.abort_on_fail:
            sys.exit(exit_code)

    settitle()
    return exit_code


# ── Global flag parsing ───────────────────────────────────────────────────────

def parse_global_flags(args):
    """Consume leading --flags and return (opts, remaining_args)."""
    opts = {
        'community_edition': False,
        'clipboard': False,
        'delay': 0,
        'debug': False,
        'dry_run': False,
        'nice': False,
        'no_scan': False,
        'output_filter': None,
    }
    i = 0
    while i < len(args):
        a = args[i]
        if a == '--ce':
            opts['community_edition'] = True
        elif a == '--clipboard':
            opts['clipboard'] = True
        elif a == '--delay':
            i += 1
            opts['delay'] = float(args[i])
        elif a == '--debug':
            opts['debug'] = True
        elif a == '--dry-run':
            opts['dry_run'] = True
        elif a == '--log':
            logfile = f'gwb_{datetime.now().strftime("%Y.%m.%d-%H.%M.%S")}.log'
            print(f'Logging to {logfile}')
            # tee-based logging could be wired up here
        elif a == '--nice':
            opts['nice'] = True
        elif a == '--no-scan':
            opts['no_scan'] = True
        elif a == '--setx':
            opts['debug'] = True
        elif a == '--wc':
            opts['output_filter'] = 'wc'
        elif a.startswith('--'):
            print(f'Unknown option {a}')
            sys.exit(1)
        else:
            break
        i += 1
    return opts, args[i:]


# ── Task dispatch loop ────────────────────────────────────────────────────────

def process_tasks(args, opts):
    # A leading /path argument means "change to ~/gw/path first". Do this before
    # sourcing gwSetEnv so the env is read from the target project directory.
    if args and args[0].startswith('/'):
        target = os.path.expanduser(f'~/gw{args[0]}')
        try:
            os.chdir(target)
        except OSError as e:
            print(e, file=sys.stderr)
            sys.exit(1)

    source_gwsetenv()

    uname         = os.environ.get('UNAME', os.uname().sysname)
    gw_appcode    = os.environ.get('GW_APPCODE', '')
    gw_port       = os.environ.get('GW_PORT', '8080')
    gw_port_debug = os.environ.get('GW_PORT_DEBUG', '8090')
    gw_db_dir     = os.environ.get('GW_DB_DIR', './db')
    gw_pl_num     = int(os.environ.get('GW_PL_NUM', '0') or '0')
    gw_start      = os.environ.get('GW_START_SCRIPT', './gwb')
    idea_home     = os.environ.get('IDEA_HOME', '')
    browser       = os.environ.get('BROWSER', 'open')
    ls_flags      = os.environ.get('LS_FLAGS', '')

    default_run_port = gw_port_debug
    try:
        with open('gradle.properties') as f:
            for line in f:
                if line.startswith('jettyPort='):
                    default_run_port = line.split('=', 1)[1].strip()
                    break
    except FileNotFoundError:
        pass
    default_debug_port = str(int(default_run_port) + 10)

    seven_zip         = '7zz' if uname == 'Darwin' else '7z'
    resources_dir     = f'{os.getcwd()}/app-{gw_appcode}/{gw_appcode}-run/build/idea/webapp'
    webserver_port    = str(int(gw_port_debug) + 1)
    webserver         = f'ruby -run -ehttpd {resources_dir} -p{webserver_port}'
    server_debug_proc = f'java .*-Dgw.port={gw_port_debug}.* com.guidewire'
    pass_icon         = f'/gw-dev/icons/{gw_appcode.upper()}_favicon.ico'

    debug      = opts['debug']
    dry_run    = opts['dry_run']
    clipboard  = opts['clipboard']
    delay      = opts['delay']
    nice       = opts['nice']
    no_scan    = opts['no_scan']
    out_filter = opts['output_filter']

    i = 0
    while i < len(args):
        arg = args[i]
        i += 1

        if '=' in arg:
            task_name, value = arg.split('=', 1)
        else:
            task_name, value = arg, ''

        notify_volume = None
        if task_name.endswith('Quiet'):
            notify_volume = 'quiet'
            task_name = task_name[:-5]
        elif task_name.endswith('Loud'):
            notify_volume = 'loud'
            task_name = task_name[:-4]

        task = Task(
            name=task_name,
            command=f'{gw_start} {task_name}',
            clipboard=clipboard,
        )

        # ── Task cases ────────────────────────────────────────────────────────

        if task_name.startswith('/'):
            task.eval_it  = True
            task.command  = f'cd ~/gw{task_name}'

        elif task_name == 'backupDb':
            comment      = f'_{value.replace(" ", "_")}' if value else ''
            db_name      = datetime.now().strftime(f'{DB_BACKUP}_%y%m%d_%H:%M:%S{comment}')
            task.command = f'{seven_zip} a {db_name} {gw_db_dir}; {seven_zip} l {db_name}.7z'

        elif task_name in ('cc', 'ccc'):
            task.command       = f"echo Error: You probably meant to say 'dev {task_name}'! && exit 1"
            task.fail_icon     = FAIL_ICON
            task.abort_on_fail = True

        elif task_name in ('clean', 'codegen', 'genDataDictionary', 'genPcfDictionary',
                           'genPcfXsd', 'genSystemData', 'genTypeInfo', 'genWsdl',
                           'genWsiLocal', 'packageCustomerDist', 'stop'):
            pass  # use default command

        elif task_name == 'cleanIdea':
            task.fail_icon = FAIL_ICON

        elif task_name == 'cleanNuke':
            ans = input('Really nuke everything? [yN] ')
            if ans.strip().lower() == 'y':
                task.command = 'git clean -dxf'
            else:
                continue

        elif task_name == 'compile':
            task.command = './gwb compile -DincludeGtest=true'

        elif task_name == 'dropDb':
            t            = 'dropDb' if gw_pl_num >= 9 else 'dropdb'
            task.command = f'{gw_start} {t}'
            task.fail_icon = FAIL_ICON

        elif task_name == 'editDisplayKey':
            task.spawn   = True
            task.command = f'idea app-{gw_appcode}/{gw_appcode}/config/locale/display.properties'

        elif task_name == 'fixgwb':
            task.command = "perl -pi.bak -e 's/^([_A-Za-z][_A-Za-z0-9]+) \\( \\)/\\1()/g' gwb"

        elif task_name == 'fixRunConfigs':
            task.command = (
                f"perl -pi.bak -e 's/-Xmx4g/-Dgw.port=8001 -Xmx4g/'"
                f" .idea/runConfigurations/Snowcat*.xml && "
                f"perl -pi.bak -e 's/port={default_run_port}/port=${{GW_PORT}}/;"
                f" s/port={default_debug_port}/port=${{GW_PORT_DEBUG}}/'"
                f" .idea/runConfigurations/*.xml; "
                "echo 'Remember to add \\${GW_PORT_DEBUG}/ to the default runconfigs!'"
            )

        elif task_name == 'fixPuml':
            if not value:
                task.command = f"echo '{task_name}' requires a file name argument! && exit 1"
            else:
                shutil.copy2(value, f'{value}.bak')
                has_skinparam = subprocess.run(
                    ['rg', 'skinparam class', value], capture_output=True
                ).returncode == 0
                if not has_skinparam:
                    header_expr = (
                        r'gsub(/^@startuml$/, %Q(@startuml\n'
                        r'!pragma useIntermediatePackages false\n'
                        r"'scale 1800 width\n"
                        r"'scale 4000 height\n"
                        r'hide empty members\n'
                        r"'left to right direction\n"
                        r"'set separator none\n"
                        r'skinparam class {\n'
                        r'  BackgroundColor<<gosu>> lightgreen\n'
                        r'  BackgroundColor<<internal>> lightblue\n'
                        r'  AttributeFontColor<<legacy>> Gray\n'
                        r'  StereotypeFontColor<<legacy>> Gray\n'
                        r'}\npackage entity #wheat {}\n))'
                    )
                    subprocess.run(['ruby', '-pi', '-e', header_expr, value])
                ruby_cmd = (
                    r"gsub(/^.*\b[gs]et(ArchivePartition|BeanVersion|CreateTime|CreateUser|ID|LoadCommandID|PublicID|Retired|RetiredValue|UpdateTime|UpdateUser)\b.*\n/, %q());"
                    r"gsub(/^.*\b((Insert|Remove|Update)Callback|__createInternalInterface|__getDelegateMap|__getInternalInterface|AbstractEditableRetireableBeanProxy|Approvable|Bean|BeanBase|DelegateLoader|EFTDataDelegate|EntityPropertyInfoReference|EntityTypeReference|EventAware|Extractable|gw\.pl\.persistence\.core\.Key|isNew|isNewlyImported|isRetired|KeyableBean|List<EventDescriptor>|remove|Retireable|touch|TransactionTAccountOperationsDelegate|Validatable|Versionable)\b.*\n/, %q());"
                    r"gsub(/^.*[~-] \{static\}.*\n/, %q());"
                    r"gsub(/^.*(DELEGATE_MAP|_DYNPROP|_EVENT|_internal)\n/, %q());"
                    r"gsub(/\b(typekey|java\.(lang|math|util))\./, %q());"
                )
                subprocess.run(['ruby', '-pi', '-e', ruby_cmd, value])
                diff = subprocess.run(['diff', '-q', value, f'{value}.bak'])
                if diff.returncode == 0:
                    os.remove(f'{value}.bak')
                    task.command = 'echo No changes made.'
                else:
                    task.command = f'wc -l {value} {value}.bak | grep -v total'

        elif task_name in ('h2', 'h2Basic'):
            jar_result = subprocess.run(
                "find .gradle ~/.gradle -type f -regex '.*/h2-[0-9.]*\\.jar' -print -quit",
                shell=True, capture_output=True, text=True, executable='/bin/zsh',
            )
            jar_file = jar_result.stdout.strip()
            db_path  = os.getcwd() + gw_db_dir.lstrip('.') + f'/{gw_appcode}'
            if task_name == 'h2':
                task.command = f"java -jar {jar_file} -url 'jdbc:h2:{db_path};IFEXISTS=TRUE'"
                print(f'\n\033[5;97;101mChange the host to 127.0.0.1.\033[0m')
            else:
                task.command = f'java -jar {jar_file}'
                print(f'\n\033[5;97;101mChange the host to 127.0.0.1, and make sure the JDBC URL is \'{db_path}\'! There is no username or password.\033[0m')

        elif task_name == 'http':
            task.spawn   = True
            task.notify  = False
            task.command = webserver

        elif task_name == 'idea':
            if gw_pl_num < 9:
                task.command = f'{gw_start} gen-ide'

        elif task_name == 'ij':
            task.spawn   = True
            task.command = f'{idea_home}/bin/idelsa.sh'

        elif task_name == 'killH2':
            port = int(default_run_port) + 2
            task.command = (
                f'process=$(lsof -i tcp:{port} -t) && '
                f'/bin/kill -9 ${{process}} 2>/dev/null && echo Killed process $process || '
                f'(echo Port {port} was not in use && exit 1)'
            )

        elif task_name == 'killHttp':
            task.command = f"pkill -f '{webserver}'"

        elif task_name == 'killPort':
            port         = value or gw_port
            task.command = (
                f'process=$(lsof -i tcp:{port} -t) && '
                f'/bin/kill -9 ${{process}} 2>/dev/null && echo Killed process $process || '
                f'(echo Port {port} was not in use && exit 1)'
            )

        elif task_name == 'killServer':
            subprocess.run(f'pgrep -af {server_debug_proc}', shell=True)
            task.command = f"{PKILL} '{server_debug_proc}'"

        elif task_name == 'killStudio':
            task.command   = f"{PKILL} '-Dstudio.sdkroot={os.getcwd()} '"
            task.fail_icon = FAIL_ICON

        elif task_name == 'lsApd':
            dirs_str     = ' '.join(APD_DIRS)
            task.command = f'find {dirs_str} -type f 2>/dev/null | xargs ls {ls_flags} -l --color'

        elif task_name == 'lsDb':
            task.notify  = False
            task.command = f'ls -l {gw_db_dir}'

        elif task_name == 'lsDiff':
            branch = value or 'origin/h-master'
            if branch == 'coins':
                branch = 'origin/feat/gcl_coinsurance'
            task.clipboard = True
            task.command   = f"git show --name-only --pretty='' {branch}.. | toIJScope"

        elif task_name == 'lsplugins':
            meta_pattern  = '.idea/settings/system/plugins/meta/*.json'
            expected_pipe = '|'.join(EXPECTED_PLUGINS)

            installed_result = subprocess.run(
                f"sed -E 's/^.*\"id\":([0-9]+).*name\":\"([^\"]+)\".*$/\\2/' {meta_pattern}"
                " | sort --unique --ignore-case",
                shell=True, capture_output=True, text=True,
            )
            installed = [l for l in installed_result.stdout.splitlines() if l.strip()]

            matched_expected = {p for p in EXPECTED_PLUGINS for ins in installed if ins.startswith(p)}
            missing_expected = [p for p in EXPECTED_PLUGINS if p not in matched_expected]

            print('\nExpected:')
            for n, p in enumerate(EXPECTED_PLUGINS, 1):
                print(f'{n:3}  {p}')

            task.command = (
                f"echo; sed -E 's/^.*\"id\":([0-9]+).*name\":\"([^\"]+)\".*$/\\2/' "
                f"{meta_pattern} | sort --unique --ignore-case | cat -n | lite '{expected_pipe}'; "
                f"echo; echo 'Missing Expected: {missing_expected}'; echo"
            )

        elif task_name == 'lsport':
            task.command = f'lsof -i tcp:{value}'

        elif task_name == 'mkpuml':
            if not value:
                task.command = f"echo '{task_name}' requires a file name argument! && exit 1"
            else:
                with open(f'{value}.puml', 'w') as f:
                    f.write(PUML_HEADER)
                task.command = f'ls {value}.puml'

        elif task_name == 'opendd':
            task.command = 'open build/cc/dictionary/data/index.html'

        elif task_name == 'pcfs':
            pcf_host     = value.removeprefix('http://') if value else f'localhost:{webserver_port}'
            task.notify  = False
            task.command = (
                f"ruby -pi -e 'gsub(%r(http://[^:]+:(null|[0-9]+)/\\w+/resources), "
                f"%q(http://{pcf_host}/resources))' /tmp/*.html && "
                f"ls -l /tmp && {browser} /tmp"
            )

        elif task_name == 'pgrepServer':
            task.command = grep_proc_str(server_debug_proc)

        elif task_name == 'pgrepStudio':
            task.command   = grep_proc_str(f'-Dstudio.sdkroot={os.getcwd()} ')
            task.fail_icon = FAIL_ICON
            print(task.command)

        elif task_name == 'pwd':
            task.command = 'pwd'

        elif task_name == 'restoreDb':
            if not value:
                task.command = f"echo '{task_name}' requires a database name argument! && exit 1"
            else:
                Path(gw_db_dir).mkdir(parents=True, exist_ok=True)
                task.command = (
                    f"{seven_zip} x -aoa -o{gw_db_dir}/.. '{value}' && "
                    f'ls -l {gw_db_dir}'
                )

        elif task_name == 'rmDb':
            task.command = f'rm -fv {gw_db_dir}/* && echo && tree {gw_db_dir}'

        elif task_name == 'scratchReport':
            query = (
                "git status --porcelain | grep 'Test\\.(gs|java)' | "
                "/usr/bin/sed 's/^[ A-Z]* //; "
                "s_^.*/g*src/__; s_^.*/g*test/__; s_/_._g; "
                "/gs$/ { s/^\"/; s/.gs$/\"/; }; "
                "/java$/ { s/java$/class/; }; "
                "s/^/.withTest(/; s/$/)/' "
            )
            print(f'query={query!r}')
            subprocess.run(query, shell=True, executable='/bin/zsh')
            task.command = query

        elif task_name == 'setBg':
            if i < len(args):
                task.command = f"itermBg '{args[i]}'"
                i += 1
            else:
                task.command = "echo 'setBg requires a background image argument' && exit 1"

        elif task_name == 'setPlVersion':
            if i < len(args):
                ver = args[i]
                i += 1
                task.command = (
                    f"perl -pi.bak -e 's/^plVersion=.*/plVersion={ver}/g' gradle.properties; "
                    f"rm gradle.properties.bak; "
                    f"git add gradle.properties; git diff gradle.properties"
                )
            else:
                task.command = "echo 'setPlVersion requires a version argument' && exit 1"

        elif task_name == 'studio':
            task.ignore_nice = True
            task.fail_icon   = FAIL_ICON
            if gw_pl_num < 9:
                task.spawn = True
            if gw_appcode == 'ccpi':
                task.command = 'idea'
            else:
                prefix       = '--no-scan ' if no_scan else ''
                task.command = f'{gw_start} {prefix}{task_name}'

        elif task_name == 'tasks':
            subprocess.run(
                f'{gw_start} tasks | sed "s/^\\([a-z].*\\)\\( - \\)/[96m\\1[0m\\2/g"',
                shell=True, executable='/bin/zsh',
            )
            sys.exit(0)

        elif task_name == 'testFail':
            fail_code    = args[i] if i < len(args) else '1'
            if i < len(args):
                i += 1
            task.command = f'time && sleep 1 && exit {fail_code}'

        elif task_name == 'testPass':
            task.command = 'time && sleep 1s && echo "done"'

        elif task_name == 'triggerTests':
            task.command = 'git commit --amend --date=now --no-edit; git push -f'

        elif task_name == 'wc':
            dirs = [f'app-{gw_appcode}']
            if os.path.isdir('appcommons'):
                dirs.append('appcommons')
            if os.path.isdir('platform'):
                dirs.append('platform')
            dirs_str     = ' '.join(dirs)
            task.command = (
                f"nice -n 15 gfind {dirs_str} -type f "
                r"\( -path '*/build/*' -o -path '*/generated/*' -o -iregex '.*test.*' -prune \) "
                r"-o \( -name '*.java' -o -name '*.gs' -o -name '*.gst' \) "
                r"-print0 | gwc --lines --files0-from=- | "
                f"xargs printf \"\\n%'9d %s\"; "
                f"echo ' from non-test files in: {dirs_str}'"
            )

        elif task_name == 'wipeApd':
            dirs_str       = ' '.join(APD_DIRS)
            potential_cmd  = f'rm -rfv {dirs_str}'
            print()
            if i >= len(args):  # final argument — no confirmation needed
                task.command = potential_cmd
            else:
                print(potential_cmd)
                ans = input(
                    f'\033[30;{warn_color}mReally delete all APD content using the above command? [yN]\033[0m '
                )
                if ans.strip().lower() == 'y':
                    task.command = potential_cmd
                else:
                    print('\nAPD content not deleted.')
                    if i < len(args):
                        cont = input(
                            f'\033[30;{warn_color}mContinue execution? [yN]\033[0m '
                        )
                        if cont.strip().lower() == 'y':
                            continue
                    remaining = args[i:]
                    msg = (f'Ignored remaining arguments: \033[1m{" ".join(remaining)}\033[0m'
                           if remaining else '')
                    log_section(fail_color, 'ABORTING Execution',
                                msg or 'No remaining arguments')
                    return

        elif task_name == 'www':
            task.command = f'open http://localhost:{gw_port_debug}/{gw_appcode}'

        else:
            task.pass_icon = pass_icon
            task.fail_icon = FAIL_ICON

        # Apply Loud/Quiet notification override
        if notify_volume == 'quiet':
            task.pass_icon = ''
            task.fail_icon = ''
        elif notify_volume == 'loud':
            task.pass_icon = pass_icon
            task.fail_icon = FAIL_ICON

        # Consume any trailing dash-prefixed arguments (e.g. -DskipTests, -x)
        while i < len(args) and args[i].startswith('-'):
            task.command += f' {args[i]}'
            i += 1

        if out_filter:
            task.command += f' | {out_filter}'

        run_task(task, dry_run=dry_run, delay=delay, nice=nice, debug=debug)


def main():
    opts, remaining = parse_global_flags(sys.argv[1:])
    if not remaining:
        print('Usage: gwb.py [--options] <task>[=value] [<task>[=value] ...]')
        sys.exit(0)
    process_tasks(remaining, opts)


if __name__ == '__main__':
    main()
