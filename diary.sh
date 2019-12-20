#!/bin/bash

eval DIARY_CONFIG="~/.tinydiary"
diary_file=""

if [ -z "$2" ]
then
    echo "no output file specified"

    if [ -e "$DIARY_CONFIG" ]
    then
        while IFS= read -r line
        do
            eval diary_file=$line
        done < "$DIARY_CONFIG"

        if [ -z $diary_file ]
        then
            echo "please specify a default diary file on line 1 of .tinydiary"
            exit 1
        fi
    else
        echo "no ~/.tinydiary"
        exit 2
    fi
else
    eval diary_file="$2"
fi

echo "writing to $diary_file"

printf "$(date)\n$1\n\n" >> "$diary_file"
