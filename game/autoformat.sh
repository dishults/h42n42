#!/usr/bin/env zsh

for file in *.eliom; do
    echo "\033[32m$file\033[0m"
    FORMATED=$(ocamlformat $file)
    echo $FORMATED > $file
done
