import os.path
import subprocess

from xonsh.built_ins import XSH

env = XSH.env or {}

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

full_cmd = ["zoxide", "init", "xonsh", "--cmd", "cd"]
XSH.builtins.execx(
    subprocess.run(full_cmd, capture_output=True).stdout.decode("utf8"),
    "exec",
    __xonsh__.ctx,
    filename="zoxide",
)

env["CASE_SENSITIVE_COMPLETIONS"] = False
env["COMMANDS_CACHE_SAVE_INTERMEDIATE"] = True
env["COMPLETION_IN_THREAD"] = True
env["ENABLE_ASYNC_PROMPT"] = True
env["HISTCONTROL"] = "erasedups"
env["MULTILINE_PROMPT"] = " "
env["UPDATE_OS_ENVIRON"] = True
env["XONSH_AUTOPAIR"] = True
env["XONSH_HISTORY_BACKEND"] = "sqlite"
env["XONSH_HISTORY_MATCH_ANYWHERE"] = True
