#!/bin/bash

print_info () {
    echo "usage: memory deck add name"
    echo "       memory deck del name"
    echo "       memory deck sel name"
    echo "       memory deck list"
    echo "       memory card add front back"
    echo "       memory card del id"
    echo "       memory card list"
}

FLASHCARD_HOME="$HOME/.memory"
DECKS="$FLASHCARD_HOME/decks"
DEFAULT="$DECKS/default"

mkdir $DEFAULT -p

if [ -z $2 ]; then print_info; exit; fi

if [[ $1 = "card" ]]
then
    if [[ $2 = "add" ]]
    then
        echo "add card"
    fi

    if [[ $2 = "del" ]]
    then
    
        echo "add card"
    fi

    if [[ $2 = "list" ]]
    then
    
        echo "add card"
    fi
fi

if [[ $1 = "deck" ]]
then
    if [[ $2 = "add" ]]
    then
        echo "add card"
    fi

    if [[ $2 = "del" ]]
    then
        echo "add card"
    fi

    if [[ $2 = "list" ]]
    then
        echo "add card"
    fi
fi
