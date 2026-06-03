#!/bin/bash
# emacs-eval.sh - Execute elisp in running Emacs via emacsclient
# Usage: ./emacs-eval.sh '(+ 1 2)'
#
# Socket discovery order:
#   1. $EMACS_SOCKET env var (explicit override)
#   2. /var/folders/*/*/T/emacs<uid>/server  (macOS real TMPDIR, survives sandbox)
#   3. ${TMPDIR}emacs<uid>/server            (standard XDG / Linux location)
#   4. lsof fallback: query the running emacs --daemon process
#   5. emacsclient default (no --socket-name) — works when $TMPDIR is not sandboxed

UID_NUM=$(id -u)

_find_socket() {
    # 1. Explicit override
    if [ -n "${EMACS_SOCKET:-}" ] && [ -S "$EMACS_SOCKET" ]; then
        echo "$EMACS_SOCKET"
        return
    fi

    # 2. macOS /var/folders glob (survives sandboxed $TMPDIR)
    local sock
    sock=$(ls /var/folders/*/*/T/emacs${UID_NUM}/server 2>/dev/null | head -1)
    if [ -S "${sock:-}" ]; then
        echo "$sock"
        return
    fi

    # 3. Standard TMPDIR location (Linux / non-sandboxed macOS)
    sock="${TMPDIR:-/tmp}emacs${UID_NUM}/server"
    if [ -S "$sock" ]; then
        echo "$sock"
        return
    fi

    # 4. lsof fallback: ask the daemon process directly
    local daemon_pid
    daemon_pid=$(pgrep -f 'emacs --daemon' 2>/dev/null | head -1)
    if [ -n "$daemon_pid" ]; then
        sock=$(lsof -a -p "$daemon_pid" -U 2>/dev/null \
               | awk 'NR>1 {print $NF}' \
               | grep -m1 '/emacs[0-9]*/server')
        if [ -S "${sock:-}" ]; then
            echo "$sock"
            return
        fi
    fi

    # 5. No socket found — return empty; caller will try emacsclient default
    echo ""
}

if [ "${BASH_SOURCE[0]}" = "$0" ]; then
    if [ $# -eq 0 ]; then
        echo "Usage: $0 '<elisp-expression>'"
        echo "Example: $0 '(message \"Hello from Claude\")'"
        exit 1
    fi

    SOCKET=$(_find_socket)

    if [ -n "$SOCKET" ]; then
        emacsclient --socket-name="$SOCKET" --eval "$1" 2>&1
    else
        # Fall back to emacsclient default lookup (works when $TMPDIR is not sandboxed)
        result=$(emacsclient --eval "$1" 2>&1)
        status=$?
        echo "$result"
        if [ $status -ne 0 ] && echo "$result" | grep -q "can't find socket"; then
            echo "Hint: set EMACS_SOCKET=<path> or start Emacs with (server-start)" >&2
        fi
        exit $status
    fi
fi
