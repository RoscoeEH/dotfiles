#!/bin/sh
if [ "$#" -eq 0 ]; then
    echo "usage: o FILE..." >&2
    exit 2
fi

for file in "$@"; do
    if [ ! -e "$file" ]; then
        echo "o: not found: $file" >&2
        continue
    fi

    mime="$(file --brief --mime-type -- "$file")"

    case "$mime" in
        text/*|\
        application/json|\
        application/xml|\
        application/x-shellscript|\
        application/x-perl|\
        application/x-python*|\
        application/x-csrc|\
        application/x-c++src|\
        application/x-rust|\
        application/x-yaml)
            emacsclient -n --alternate-editor="" -- "$file"
            ;;

        application/pdf)
            zathura -- "$file" >/dev/null 2>&1 &
            ;;

        image/*)
            feh -- "$file" >/dev/null 2>&1 &
            ;;

        video/*)
            mpv -- "$file" >/dev/null 2>&1 &
            ;;

        audio/*)
            mpv -- "$file" >/dev/null 2>&1 &
            ;;

        text/html|application/xhtml+xml)
            xdg-open "$file" >/dev/null 2>&1 &
            ;;

        *)
            xdg-open "$file" >/dev/null 2>&1 &
            ;;
    esac
done
