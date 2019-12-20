# usage: diary [DIARY FILE] [-u] DIARY ENTRY
# place a .tinydiary in your home directory with your desired default
# diary file
#
#!/bin/bash

eval DIARY_CONFIG="~/.tinydiary"
diary_file=
text=

while :;
do
    if [ -z "$1" ]; then break; fi

    if [[ "$1" = "-u" ]]
    then
        echo "undo"
        exit 0
    fi

    if [ -z "$text" ]
    then
        text=$1
    else
        eval filename="$1"
        diary_file="$filename"
    fi

    shift
done

if [ -z "$text" ]
then
    echo "no text provided"
    exit 1
fi

if [ -z $diary_file ]
then
    if [ -e "$DIARY_CONFIG" ]
    then
        while IFS= read -r line
        do
            eval diary_file=$line
        done < "$DIARY_CONFIG"

        if [ -z $diary_file ]
        then
            echo "please specify a default diary file in .tinydiary"
            exit 1
        fi
    else
        echo "no ~/.tinydiary"
        exit 2
    fi
fi

printf "$(date)\n$text\n\n" | tee -a "$diary_file"
printf ">> $diary_file\n"
