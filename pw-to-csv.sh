#!/bin/bash
#
# converts my old "q" password file to csv format, 
# to be imported to keepass
# the format of "q" is inconsistent but usually
# website/ service
# username/ e-mail
# password
# extra details

# file to read from
filename=$1
# file to write to
target=$2
# counts which line of a record we are processing (0=website, 1=username)
line_num=0
# parts of a particular record (website, account, password) etc 
parts=()

if [[ ! -f $filename ]]
then
    echo "file $filename does not exist"
    exit 1
fi

if [[ -f $target ]]
then
    echo "file $target already exists. overwrite (y/n)?"
    exit 2
fi

while IFS= read -r line
do
    if [[ ${#line} = 0 ]]
    then
        line_num=0
    else
        line_num=$((line_num+1))
    fi
done < "$filename"




