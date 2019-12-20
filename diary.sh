# usage: diary [DIARY FILE] [-u] DIARY ENTRY
# place a .tinydiary in your home directory with your desired default
# diary file
#
#!/bin/bash

eval DIARY_CONFIG="~/.tinydiary"
diary_file=""

for arg in "$@"
do
    if [ "$arg" -eq ""
done

if [ -z "$1" ]
then
    echo "no text provided"
    exit 3
fi

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
            echo "please specify a default diary file"
            exit 1
        fi
    else
        echo "no ~/.tinydiary"
        exit 2
    fi
else
    eval diary_file="$2"
fi

printf "$(date)\n$1\n\n" | tee -a "$diary_file"
