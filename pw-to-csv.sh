#!/bin/bash
#
# converts my old "q" password file to csv format, 
# to be imported to keepass
# the format of "q" is inconsistent but usually
# website/ service
# username/ e-mail
# password
# extra details

if [[ ! -f $1 ]]
then
    echo "$1 does not exist"
    exit 1
fi


