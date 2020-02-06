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
CURRENT_FILE="$MEM_HOME/.current_deck"
CURRENT=$(head -n 1 $CURRENT_FILE)

mkdir $MEM_HOME -p
touch $DEFAULT

if [ ! -f $CURRENT_FILE ]
then
    echo "default" > $CURRENT_FILE
fi

if ([ -z $3 ] && [ "$2" != "list" ]); then print_info; exit; fi

if [[ $1 = "card" ]]
then
    if [[ $2 = "add" ]]
    then
        echo "add card to $CURRENT"
    fi

    if [[ $2 = "del" ]]
    then
        echo ""
    fi

    if [[ $2 = "list" ]]
    then
        echo ""
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
        rm "$MEM_HOME/$3"
    fi
fi
