#!/bin/bash

dunstctl history | jq -r '
  .data[0][] |
  "\(.appname.data): \(.summary.data)\n\(.body.data)\n---"
'
