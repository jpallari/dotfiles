#!/usr/bin/env bash
set -euo pipefail

base64_decode() {
    base64 -d 2>/dev/null || true
}

main() {
    local line
    read -r line

    local first second
    first=$(echo "$line" | cut -d '.' -f 1 | base64_decode | jq)
    second=$(echo "$line" | cut -d '.' -f 2 | base64_decode | jq)

    echo "$first"
    echo "$second"
}

main
