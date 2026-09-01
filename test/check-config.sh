#!/usr/bin/env bash
# Static checks for this Emacs configuration.
#
# Offline and side-effect free: it never starts a server, never fetches a
# package, and never writes into straight/ or eln-cache/. It exists so that
# every change can be gated before the slow, stateful test -- actually
# launching Emacs.
#
#   ./test/check-config.sh
#   EMACS=/path/to/Emacs ./test/check-config.sh

set -uo pipefail

EMACS=${EMACS:-/Applications/Emacs.app/Contents/MacOS/Emacs}
root=$(cd "$(dirname "$0")/.." && pwd)
log=$root/test/check-config.log
fail=0

pass() { printf '  \033[32mok\033[0m   %s\n' "$1"; }
bad()  { printf '  \033[31mFAIL\033[0m %s\n' "$1"; fail=1; }

if [[ ! -x $EMACS ]]; then
  echo "check-config: no Emacs at $EMACS" >&2
  exit 1
fi

: >"$log"

echo "== shell shims =="
shopt -s nullglob
shims=("$root"/bin/*)
if (( ${#shims[@]} == 0 )); then
  pass "no shims to check"
fi
for shim in "${shims[@]}"; do
  if bash -n "$shim" 2>>"$log"; then pass "bash -n $(basename "$shim")"
  else bad "bash -n $(basename "$shim")"; fi
  if [[ -x $shim ]]; then pass "executable $(basename "$shim")"
  else bad "$(basename "$shim") is not executable"; fi
done

echo "== byte-compile =="
# test/compile-prelude.el registers a no-op :straight keyword so use-package
# actually parses these forms -- without it use-package aborts on the
# unrecognized keyword and the bodies are never compiled at all.
blocking='is an obsolete|Malformed|Invalid|misplaced|wrong number of arguments|Wrong type|Error \(use-package\)'
for f in early-init.el init.el lisp/my-hyper.el lisp/my-op.el; do
  [[ -f $root/$f ]] || continue
  out=$("$EMACS" -Q --batch \
          -l "$root/test/compile-prelude.el" \
          --eval '(setq byte-compile-dest-file-function
                        (lambda (_) (make-temp-file "chk" nil ".elc")))' \
          -f batch-byte-compile "$root/$f" 2>&1)
  printf '\n--- %s ---\n%s\n' "$f" "$out" >>"$log"
  if grep -Eq "$blocking" <<<"$out"; then
    bad "$f has blocking warnings (see test/check-config.log)"
    grep -E "$blocking" <<<"$out" | sort -u | sed 's/^/       /'
  else
    pass "$f compiles clean"
  fi
done

echo "== hyper keymap =="
if [[ -f $root/lisp/my-hyper.el ]]; then
  # Loading the file binds every key for real. keymap-global-set signals on
  # an invalid key description, so a clean load already proves key-valid-p
  # for the whole layer; then we assert the bindings resolve.
  out=$("$EMACS" -Q --batch -l "$root/lisp/my-hyper.el" \
        -l "$root/test/hyper-assertions.el" 2>&1)
  printf '\n--- hyper ---\n%s\n' "$out" >>"$log"
  if grep -q '^hyper: ok' <<<"$out"; then
    pass "$(grep '^hyper: ok' <<<"$out")"
  else
    bad "hyper keymap assertions"
    sed 's/^/       /' <<<"$out"
  fi
else
  pass "lisp/my-hyper.el not present yet"
fi

echo "== 1Password reader =="
if [[ -f $root/lisp/my-op.el ]]; then
  # Drives my-op.el against a stub CLI in a temp directory -- it never runs
  # the real `op', so this stays offline and cannot raise a Touch ID prompt.
  out=$("$EMACS" -Q --batch -l "$root/lisp/my-op.el" \
        -l "$root/test/op-assertions.el" 2>&1)
  printf '\n--- op ---\n%s\n' "$out" >>"$log"
  if grep -q '^op: ok' <<<"$out"; then
    pass "$(grep '^op: ok' <<<"$out")"
  else
    bad "1Password reader assertions"
    sed 's/^/       /' <<<"$out"
  fi
else
  pass "lisp/my-op.el not present yet"
fi

echo
if (( fail )); then
  echo "check-config: FAILED (details in test/check-config.log)"
else
  echo "check-config: all checks passed"
fi
exit "$fail"
