#!/bin/bash
# usage: worse project [Xh|Ym]
#

eval worse_file="~/.workhorse"
project=$1
time=$2
append=0

function show_help {
    echo "usage: worse project HOURShMINSm"
    echo "example: worse building ikea table 1h200m"
    echo "example: worse building ikea table -1h"
    exit 0
}

if [[ -z "$1" || -z "$2" ]]
then
    show_help
fi

if [[ "$1" = "help" ]]
then
    show_help
fi

touch $worse_file

if [ ! -f $worse_file ]
then
    echo "please create config file: $worse_file"
    exit 1
fi

function process_line {
    IFS=':'; read -r -a parts <<< "$line"
    c=0

    for i in "${parts[@]}"; do
        echo $i $c
        c=$((c + 1 % 3))
    done
}

found_data=0

while IFS= read -r line
do
    if [ $found_data = 1 ]; then
        process_line
    fi

    if [ "$line" = "%% data" ]; then
        found_data=1
    fi
done < "$worse_file"

if [ $append -eq 0 ]; then
    output="$(date)\n$text\n\n"
else
    output="$text\n\n"
fi

# printf "$output" | tee -a "$diary_file"
printf "$output"
# printf ">> $diary_file\n"
