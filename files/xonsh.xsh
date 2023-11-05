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


# emacs-eat integration
# See also:
# https://codeberg.org/akib/emacs-eat/src/branch/master/integration
# https://github.com/jnoortheen/xontrib-term-integrations

import base64
import subprocess
import sys

from xonsh.built_ins import XSH

env = XSH.env or {}
eat_integration_enabled = False


def string_base64(string: str) -> str:
    string_b = string.encode("utf8")
    string_b64 = base64.b64encode(string_b)
    string_s64 = string_b64.decode("utf8")
    return string_s64


if not eat_integration_enabled and env["TERM"][0:4] == "eat-":
    eat_integration_enabled = True

    env["PROMPT"] = (
        "\001\033]51;e;B\033\\\002%s\001\033]51;e;C\033\\\002" % env["PROMPT"]
    )
    env["MULTILINE_PROMPT_PRE"] = "\001\033]51;e;D\033\\\002"
    env["MULTILINE_PROMPT_POS"] = "\001\033]51;e;E\033\\\002"

    @XSH.builtins.events.on_postcommand
    def eat_command_pos(rtn=0, **_) -> None:
        sys.stdout.write("\033]51;e;H;%i\033\\" % rtn)
        sys.stdout.write("\033]51;e;J\033\\")

    @XSH.builtins.events.on_chdir
    def eat_on_chdir(olddir: str, newdir: str, **_) -> None:
        sys.stdout.write(
            "\033]51;e;A;%s;%s\033\\"
            % (string_base64(env.get("HOSTNAME", "")), string_base64(newdir))
        )
