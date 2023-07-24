# FIXME: Xonsh bug?
aliases["guix"] = $(which guix)

# https://github.com/TwoPizza9621536/zsh-exa
aliases["ls"] = "exa -Fgh --color-scale --git --group-directories-first --icons"
aliases["lD"] = "ls -D"
aliases["lS"] = "ls -1"
aliases["ll"] = "ls -l"
aliases["la"] = "ll -a"
aliases["lA"] = "ll --sort=acc"
aliases["lC"] = "ll --sort=cr"
aliases["lM"] = "ll --sort=mod"
aliases["lS"] = "ll --sort=size"
aliases["lX"] = "ll --sort=ext"
aliases["llm"] = "lM"
aliases["l"] = "la -a"
aliases["lsa"] = "l"
aliases["lx"] = "l -HSUimu"
aliases["lxa"] = "lx -@"
aliases["lt"] = "ls -T"
aliases["tree"] = "lt"

# GnuPG
$GPG_TTY = $(tty).strip()
gpg-connect-agent updatestartuptty /bye > /dev/null

# Zoxide
execx($(zoxide init xonsh --cmd cd), 'exec', __xonsh__.ctx, filename='zoxide')

$CASE_SENSITIVE_COMPLETIONS = False
$COMMANDS_CACHE_SAVE_INTERMEDIATE = True
$COMPLETION_IN_THREAD = True
$ENABLE_ASYNC_PROMPT = True
$HISTCONTROL = 'erasedups'
$UPDATE_OS_ENVIRON = True
$XONSH_AUTOPAIR = True
$XONSH_HISTORY_BACKEND = 'sqlite'
$XONSH_HISTORY_MATCH_ANYWHERE = True
