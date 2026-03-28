#!/usr/bin/env python3
import sys


# Fixes spacing around math operators and commas and such
def fix_spacing(line):
    result = []
    i = 0
    in_string = False
    string_char = None

    while i < len(line):
        char = line[i]

        # Handle strings
        if char in "\"'":
            if not in_string:
                in_string = True
                string_char = char
                result.append(char)
            elif char == string_char:
                in_string = False
                result.append(char)
            else:
                result.append(char)
            i += 1
            continue

        if in_string:
            result.append(char)
            i += 1
            continue

        # commas
        if char == ",":
            result.append(", ")
            i += 1
            while i < len(line) and line[i] == " ":
                i += 1
            continue

        # Add space between } and (
        if char == ")" and i + 1 < len(line) and line[i + 1] == "{":
            result.append(") {")
            i += 2
            continue

        # Handle math operators
        if char in "*/-+=":
            if result and result[-1] != " ":
                result.append(" ")
            result.append(f"{char} ")
            i += 1

            while i < len(line) and line[i] == " ":
                i += 1
            continue

        result.append(char)
        i += 1

    return "".join(result)


def format_scad(code, indent_size=2):
    lines = code.split("\n")
    result = []
    indent_level = 0
    in_multiline_comment = False

    for line in lines:
        stripped = line.strip()

        # Remove consecutive blank lines
        if not stripped:
            if result and result[-1] != "":
                result.append("")
            continue

        # Multi-line comments
        if stripped.startswith("/*") or in_multiline_comment:
            if not in_multiline_comment:
                result.append(" " * (indent_level * indent_size) + stripped)
                in_multiline_comment = "*/" not in stripped
            else:
                result.append(stripped)
                if "*/" in stripped:
                    in_multiline_comment = False
            continue

        # Regular comments
        if stripped.startswith("//"):
            result.append(" " * (indent_level * indent_size) + stripped)
            continue

        # Apply spacing
        stripped = fix_spacing(stripped)

        starts_with_close = stripped.startswith("}")
        current_indent = max(0, indent_level - 1) if starts_with_close else indent_level

        result.append(" " * (current_indent * indent_size) + stripped)

        # Count braces to update indent level
        open_braces = 0
        close_braces = 0
        in_string = False
        escape_next = False
        string_char = None

        for char in stripped:
            if escape_next:
                escape_next = False
                continue
            if char == "\\":
                escape_next = True
                continue
            if char in "\"'":
                if not in_string:
                    in_string = True
                    string_char = char
                elif char == string_char:
                    in_string = False
                continue
            if in_string:
                continue
            if char == "{":
                open_braces += 1
            elif char == "}":
                close_braces += 1

        indent_level = max(0, indent_level + open_braces - close_braces)

    # Remove trailing blank lines
    while result and result[-1] == "":
        result.pop()

    return "\n".join(result) + "\n" if result else ""


if __name__ == "__main__":
    if len(sys.argv) != 2:
        print(f"Usage: {sys.argv[0]} <file.scad>", file=sys.stderr)
        sys.exit(1)

    filepath = sys.argv[1]

    with open(filepath, "r") as f:
        code = f.read()

    formatted = format_scad(code)

    with open(filepath, "w") as f:
        f.write(formatted)
