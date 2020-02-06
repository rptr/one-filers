#!/bin/bash

print_info () {
    echo "usage: memory deck add name"
    echo "       memory deck del name"
    echo "       memory deck sel name"
    echo "       memory deck list"
    echo "       memory card add front back"
    echo "       memory card del id"
    echo "       memory card list [deck]"
}

deck_select () {
    echo "select deck $1"
}

MEM_HOME="$HOME/.memory"
DEFAULT="$MEM_HOME/default"
CURRENT_STORE="$MEM_HOME/.current_deck"
CURRENT=$(head -n 1 $CURRENT_STORE)
CURRENT_FILE="$MEM_HOME/$CURRENT"

mkdir $MEM_HOME -p

if [ ! -f $DEFAULT ]; then
    echo "" > $DEFAULT
fi

if [ ! -f $CURRENT_STORE ]
then
    echo "default" > $CURRENT_STORE
fi

if ([ -z $3 ] && [ "$2" != "list" ]); then print_info; exit; fi

if [[ $1 = "card" ]]
then
    if [[ $2 = "add" ]]
    then
        sed -i.tmp "1s/^/$3\n/" $CURRENT_FILE
    fi

    if [[ $2 = "del" ]]
    then
        sed -i.tmp "/$3\$/{
            N
            s/$3\n//
        }" $CURRENT_FILE
    fi

    if [[ $2 = "list" ]]
    then
        cat "$MEM_HOME/$CURRENT"
    fi
fi

if [[ $1 = "deck" ]]
then
    if [[ $2 = "add" ]]
    then
        touch "$MEM_HOME/$3"
    fi

    if [[ $2 = "del" ]]
    then
        rm "$MEM_HOME/$3"
    fi

    if [[ $2 = "list" ]]
    then
        for deck in "$MEM_HOME/*"; do
            echo $deck
        done
    fi

    if [[ $2 = "sel" ]]
    then
        exists=$(find $MEM_HOME -name "$3" )

        if [ ! -n "$exists" ]; then
            echo "no such deck"
            exit
        fi

        echo "$3" > $CURRENT_STORE
    fi
fi
