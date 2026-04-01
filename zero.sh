#!/bin/bash
# Run a compiled zero program (.py or .ts)
# Usage: ./zero.sh [options] <program> [args...]
# Searches output/ directories for the file if not found directly.
# Runs in background via nohup by default.
set -e

if [ $# -lt 1 ]; then
    echo "Usage: ./zero.sh [--fg|--stop|--restart] <program.py|program.ts|name> [args...]"
    exit 1
fi

# parse flags
MODE="start"
case "$1" in
    --fg)      MODE="fg"; shift ;;
    --stop)    MODE="stop"; shift ;;
    --restart) MODE="restart"; shift ;;
    --test)    MODE="test"; shift ;;
esac

prog="$1"
shift || true

# if no extension, default to .py
case "$prog" in
    *.py|*.ts) ;;
    *) prog="${prog}.py" ;;
esac

# search for the file if not found directly
find_program() {
    local name="$1"
    [ -f "$name" ] && echo "$name" && return
    for d in */output/; do
        [ -f "${d}${name}" ] && echo "${d}${name}" && return
    done
    local found=$(find . -name "$name" -path "*/output/*" 2>/dev/null | head -1)
    [ -n "$found" ] && echo "$found" && return
    return 1
}

resolved=$(find_program "$prog") || {
    echo "Error: cannot find $prog"
    exit 1
}

stop_program() {
    pkill -f "$resolved" 2>/dev/null && echo "stopped $resolved" || echo "$resolved not running"
}

start_program() {
    pkill -f "$resolved" 2>/dev/null || true
    sleep 0.3
    case "$resolved" in
        *.py)
            logfile="${resolved%.py}.log"
            nohup python3 "$resolved" "$@" > "$logfile" 2>&1 &
            ;;
        *.ts)
            logfile="${resolved%.ts}.log"
            nohup npx tsx "$resolved" "$@" > "$logfile" 2>&1 &
            ;;
    esac
    echo "running $resolved (pid $!) — log: $logfile"
}

case "$MODE" in
    test)
        case "$resolved" in
            *.py) python3 "$resolved" --test "$@" ;;
            *.ts) npx tsx "$resolved" --test "$@" ;;
        esac
        ;;
    stop)
        stop_program
        ;;
    restart)
        stop_program
        sleep 0.5
        start_program "$@"
        ;;
    fg)
        pkill -f "$resolved" 2>/dev/null || true
        sleep 0.3
        echo "running $resolved (foreground)"
        case "$resolved" in
            *.py) python3 "$resolved" "$@" ;;
            *.ts) npx tsx "$resolved" "$@" ;;
        esac
        ;;
    start)
        start_program "$@"
        ;;
esac
