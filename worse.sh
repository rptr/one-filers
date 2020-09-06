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

# if [[ "$1" = "help" ]]
# then
#     show_help
# fi

if [[ -z "$1" ]]
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

# worse list
if [[ $1 = "list" ]]; then
    for ((i = 0 ; i < ${#project_names[@]} ; i++)); do
        name=${project_names[i]}
        hours=${project_hours[i]}
        minutes=${project_minutes[i]}
        printf "%s\t\t\t\t%dH %dM\n" $name $hours $minutes
    done

    exit 0
fi

if [[ -z "$2" ]]; then
    show_help
fi


# get the full project name
#
name_len=$(($#))
new_project=''
for ((i = 1 ; i < name_len ; i++)); do
    new_project+="${!i} "
done
new_project=$(echo $new_project | sed 's/ *$//')

# get the new hours and minutes
#
time=${!name_len}

hours=`expr "$time" : '\([0-9]*\)'`
minute_start=${#hours}
minute_start=$((minute_start+1))
minutes=`expr "${time:$minute_start}" : '\([0-9]*\)'`

echo "we are adding $hours hours and $minutes minutes $minute_start"

# try to see if we have the project on file
#
project_index=-1
i=0
for project in "${project_names[@]}"; do
    # project is already on file
    if [[ "$project" = "$new_project" ]]; then
        project_index=$i
        break
    fi

    i=$((i+1))
done

# we don't have the project on file
#
if [[ $project_index -eq -1 ]]; then
    echo "new project"
    project_names+=($new_project)
    project_hours+=($)

# we do have it on file
#
else
    echo "adding to project"
    echo ${project_hours[$project_index]}
    echo ${project_minutes[$project_index]}
fi

# printf "$output" | tee -a "$diary_file"
printf "$output"
# printf ">> $diary_file\n"










