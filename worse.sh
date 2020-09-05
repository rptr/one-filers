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

function print_project {
    IFS=':'; read -r -a parts <<< "$line"
    echo "${parts[0]}: ${parts[1]}H ${parts[2]}M"
}

# display a list of all projects and their hours
if [[ "$1" = "list" ]]
then
    while IFS= read -r line
    do
        if [[ $found_data = 1 ]]; then
            print_project
        fi

        if [[ "$line" = "%% data" ]]; then
            found_data=1
        fi
    done < "$worse_file"

    exit 0
fi

# if [[ "$1" = "help" ]]
# then
#     show_help
# fi

if [[ -z "$1" || -z "$2" ]]
then
    show_help
fi

touch $worse_file

if [ ! -f $worse_file ]
then
    echo "can't create config file: $worse_file"
    exit 1
fi




#
#
# Read .workhorse file to prepare for adding new hours
#
#
found_data=0
project_names=()
project_hours=()
project_minutes=()

# process one line from .workhorse
# e.g. "build my shed:1:20"
# means you spent 1h20m building your shed
function process_line {
    IFS=':'; read -r -a parts <<< "$line"
    c=0

    for i in "${parts[@]}"; do
        if [[ $c = 0 ]]; then
            project_names+=($i)
        elif [[ $c = 1 ]]; then
            project_hours+=($i)
        else 
            project_minutes+=($i)
        fi

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

for ((i = 0 ; i < ${#project_names[@]} ; i++)); do
    name=${project_names[i]}
    hours=${project_hours[i]}
    minutes=${project_minutes[i]}
    printf "%s\t\t\t\t%dH %dM\n" $name $hours $minutes
done

# printf "$output" | tee -a "$diary_file"
printf "$output"
# printf ">> $diary_file\n"
