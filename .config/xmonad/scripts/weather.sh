#!/bin/bash

while true; do
		curl wttr.in
    read -r -n 1 -s input
    if [[ $input = "q" ]] || [[ $input = "Q" ]] 
        then break 
    fi
done
