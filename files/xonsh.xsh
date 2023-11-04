import os.path

# FIXME: Xonsh bug?
guix_path = os.path.expanduser("~/.config/guix/current/bin/guix")
if os.path.isfile(guix_path):
    aliases["guix"] = guix_path

aliases["clp"] = "wl-copy -n"
aliases["ls"] = [
    "exa",
    "--classify",
    "--color-scale",
    "--git",
    "--group",
    "--group-directories-first",
    "--icons",
]
aliases["ll"] = "ls -l"
aliases["la"] = "ll -a"

execx($(zoxide init xonsh --cmd cd), 'exec', __xonsh__.ctx, filename='zoxide')

$CASE_SENSITIVE_COMPLETIONS = False
$COMMANDS_CACHE_SAVE_INTERMEDIATE = True
$COMPLETION_IN_THREAD = True
$ENABLE_ASYNC_PROMPT = True
$HISTCONTROL = "erasedups"
$MULTILINE_PROMPT = " "
$UPDATE_OS_ENVIRON = True
$XONSH_AUTOPAIR = True
$XONSH_HISTORY_BACKEND = "sqlite"
$XONSH_HISTORY_MATCH_ANYWHERE = True
