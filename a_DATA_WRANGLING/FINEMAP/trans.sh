#!/bin/bash

trans=$(($(head -n 1 "$1" | grep -o , | wc -l)+1))
for ((i=1; i<="$trans"; i++))
do cut -d,  -f"$i" "$1" | paste -s -d,
done