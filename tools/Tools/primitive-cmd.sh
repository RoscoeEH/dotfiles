#!/usr/bin/env bash
set -euo pipefail

REPO="$HOME/Documents/riscv_pim"
TARGET="//hardware:primitive"

usage() {
  cat <<'EOF'
Usage:
  primitive help
  primitive list
  primitive reserve <1|2> [method/reason...]
  primitive release <1|2>

Examples:
  primitive reserve 2
  primitive reserve 1 "debugging boot failure"
  primitive release 2
  primitive list
EOF
}

require_tb() {
  case "${1:-}" in
    1|2) printf 'testbench%s\n' "$1" ;;
    *)
      echo "error: expected testbench number 1 or 2" >&2
      usage >&2
      exit 2
      ;;
  esac
}

run_bazel() {
  (
    cd "$REPO"
    bazel run "$TARGET" -- "$@"
  )
}

cmd="${1:-}"
shift || true

case "$cmd" in
  help|-h|--help)
    run_bazel --help
    ;;

  list)
    run_bazel hardware list
    ;;

  reserve)
    tb="$(require_tb "${1:-}")"
    shift || true

    if [ "$#" -gt 0 ]; then
      reason="$*"
    else
      reason="Manual checkout by Roscoe at $(date '+%Y-%m-%d %H:%M:%S %Z')"
    fi

    run_bazel reservations create "$tb" "$reason"
    ;;

  release)
    tb="$(require_tb "${1:-}")"
    run_bazel reservations release "$tb"
    ;;

  *)
    usage >&2
    exit 2
    ;;
esac
