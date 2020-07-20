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

if [ -z "$1" ]
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


if [ $append -eq 0 ]; then
    output="$(date)\n$text\n\n"
else
    output="$text\n\n"
fi

printf "$output" | tee -a "$diary_file"
printf ">> $diary_file\n"
