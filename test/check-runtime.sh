#!/usr/bin/env bash
# The stateful half of the test suite: this one actually launches Emacs.
#
# test/check-config.sh is the offline gate and deliberately never starts
# Emacs. This is the other half, for everything that does not exist until
# init.el has run and the packages have loaded -- the order of
# `after-change-major-mode-hook' above all, which no amount of
# `emacs -Q --batch' can reach.
#
# Emacs is launched the way the Dock and Spotlight launch it: through env -i
# with a bare PATH. That single detail is the reason this file exists. The
# modeline reported the wrong Ruby version for as long as the indicator has
# been on, and looked correct every single time it was checked from a
# terminal -- an interactive zsh has run `mise activate', so the inherited
# PATH already carries a mise ruby and masks the bug outright.
#
#   ./test/check-runtime.sh
#   EMACS=/path/to/Emacs ./test/check-runtime.sh
#
# Two implementation details worth knowing:
#
#   * script(1), because `emacs -nw' needs a pty and this runs from a
#     non-interactive shell.
#   * A TMPDIR of its own, because init.el calls `server-start'. The server
#     socket lives under TMPDIR, so an isolated one is what keeps this from
#     colliding with -- or deleting the socket of -- a real Emacs session.
#
# Unlike check-config.sh this is NOT side-effect free: it loads the real
# configuration, so straight.el may rebuild a package and eln-cache/ may
# grow. It fetches nothing and writes nothing outside this repository.

set -uo pipefail

EMACS=${EMACS:-/Applications/Emacs.app/Contents/MacOS/Emacs}
root=$(cd "$(dirname "$0")/.." && pwd)
log=$root/test/check-runtime.log
fail=0

pass() { printf '  \033[32mok\033[0m   %s\n' "$1"; }
bad()  { printf '  \033[31mFAIL\033[0m %s\n' "$1"; fail=1; }

if [[ ! -x $EMACS ]]; then
  echo "check-runtime: no Emacs at $EMACS" >&2
  exit 1
fi
if [[ ! -x /usr/bin/script ]]; then
  echo "check-runtime: no script(1) at /usr/bin/script" >&2
  exit 1
fi

: >"$log"

tmp=$(mktemp -d "${TMPDIR:-/tmp}/emacs-check-runtime.XXXXXX")
trap 'rm -rf "$tmp"' EXIT

echo "== modeline env indicators =="
report=$tmp/modeline-env

# PATH is /usr/bin:/bin:/usr/sbin:/sbin and nothing else: no mise, no
# homebrew, no shims -- exactly what a GUI Emacs inherits. init.el puts
# /opt/homebrew/bin back on `exec-path' itself, which is how mise is found
# at all.
/usr/bin/script -q /dev/null /usr/bin/env -i \
  HOME="$HOME" \
  TMPDIR="$tmp" \
  TERM=xterm-256color \
  LANG="${LANG:-en_US.UTF-8}" \
  SHELL=/bin/zsh \
  PATH=/usr/bin:/bin:/usr/sbin:/sbin \
  MODELINE_ENV_REPORT="$report" \
  "$EMACS" -nw -l "$root/test/modeline-env-assertions.el" >/dev/null 2>&1

if [[ -s $report ]]; then
  printf '\n--- modeline env ---\n' >>"$log"
  cat "$report" >>"$log"
  if grep -q '^modeline-env: ok' "$report"; then
    pass "$(grep '^modeline-env: ok' "$report")"
  else
    bad "modeline env assertions"
    sed 's/^/       /' "$report"
  fi
else
  bad "Emacs produced no report -- it died before the assertions ran"
fi

echo
if (( fail )); then
  echo "check-runtime: FAILED (details in test/check-runtime.log)"
else
  echo "check-runtime: all checks passed"
fi
exit "$fail"
