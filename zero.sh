#!/bin/bash
# Run a compiled zero program (.py or .ts) with arguments
set -e

if [ $# -lt 1 ]; then
    echo "Usage: ./zero.sh <program.py|program.ts> [args...]"
    exit 1
fi

prog="$1"
shift

case "$prog" in
    *.py)  python3 "$prog" "$@" ;;
    *.ts)  npx tsx "$prog" "$@" ;;
    *)     echo "Unknown extension: $prog"; exit 1 ;;
esac
