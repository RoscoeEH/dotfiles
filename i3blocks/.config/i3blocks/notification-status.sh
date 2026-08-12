#!/bin/bash

CRITICALS_FILE="$HOME/.config/dunst/dunst-nc-criticals.toml"

if dunstctl is-paused | grep -q true; then
    echo "DND"
    echo ""
    echo "#5a8ffa"
else
    history_json="$(dunstctl history 2>/dev/null)"

    result="$(
        HISTORY_JSON="$history_json" python3 - "$CRITICALS_FILE" <<'PY'
import json
import os
import re
import sys

try:
    import tomllib
except ModuleNotFoundError:
    print("0 0")
    sys.exit(0)

rules_path = sys.argv[1]

try:
    with open(rules_path, "rb") as f:
        rules = tomllib.load(f)
except Exception:
    print("0 0")
    sys.exit(0)

try:
    data = json.loads(os.environ.get("HISTORY_JSON", ""))
except Exception:
    print("0 0")
    sys.exit(0)

def field(notification, name):
    value = notification.get(name, "")

    if isinstance(value, dict):
        value = value.get("data", "")

    if value is None:
        return ""

    return str(value)

def is_notification(obj):
    return (
        isinstance(obj, dict)
        and "appname" in obj
        and ("summary" in obj or "body" in obj)
    )

def walk(obj):
    if isinstance(obj, dict):
        if is_notification(obj):
            yield obj
        for value in obj.values():
            yield from walk(value)
    elif isinstance(obj, list):
        for item in obj:
            yield from walk(item)

def matches(notification, rule):
    appname = field(notification, "appname")
    subject = field(notification, "summary")
    body = field(notification, "body")

    if "appname" in rule and appname != rule["appname"]:
        return False

    if "subject_regexp" in rule and not re.search(rule["subject_regexp"], subject):
        return False

    if "body_regexp" in rule and not re.search(rule["body_regexp"], body):
        return False

    return True

notifications = list(walk(data))
count = len(notifications)
has_critical = any(
    any(matches(notification, rule) for rule in rules.values())
    for notification in notifications
)

print(count, 1 if has_critical else 0)
PY
    )"

    count="$(printf '%s\n' "$result" | awk '{print $1}')"
    has_critical="$(printf '%s\n' "$result" | awk '{print $2}')"

    count="${count:-0}"
    has_critical="${has_critical:-0}"

    echo "$count"
    echo ""

    if [ "$has_critical" -eq 1 ]; then
        echo "#FF0000"
    else
        echo "#5a8ffa"
    fi
fi
