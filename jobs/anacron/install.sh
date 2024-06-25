#!/bin/bash
JOBS_FILE="$(pwd)/jobs/anacron/anacrontab"
ANACRONTAB_FILE="/etc/anacrontab"

if [ $(id -u) -ne 0 ]; then
    echo "Error: Script must be run as root" >&2
    exit 1
fi

if [ ! -f "${ANACRONTAB_FILE}" ]; then
    echo "'${ANACRONTAB_FILE}' does not exist. Creating ..."
    touch "${ANACRONTAB_FILE}"
fi

line_exists() {
    local line="$1"
    grep -qxF "${line}" "${ANACRONTAB_FILE}"
}

while IFS= read -r line; do
    if [[ -n "${line}" && ! "${line}" =~ ^\s*# ]]; then
        if ! line_exists "${line}"; then
            echo "${line}" >> "${ANACRONTAB_FILE}"
            echo "Added to anacrontab: ${line}"
        else
            echo "Skipped (already exists): ${line}"
        fi
    fi
done < "${JOBS_FILE}"

echo "Anacron jobs installation complete."

