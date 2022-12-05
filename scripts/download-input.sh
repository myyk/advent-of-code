#!/bin/bash


INPUT_FILE=src/main/resources/samples/$1/day$2.txt
if [ -f "$INPUT_FILE" ]; then
    echo "$INPUT_FILE exists. Not downloading. Delete it first if you want to recreate it."
else 
    echo "Downloading input file to $INPUT_FILE"
    curl https://adventofcode.com/$1/day/$2/input --cookie "session=$ADVENT_SESSION" --create-dirs --output $INPUT_FILE
fi

day=$(printf "%02d" $2)
SRC_FILE=src/main/scala/com/github/myyk/advent$1/day$day.worksheet.sc
if [ -f "$SRC_FILE" ]; then
    echo "$SRC_FILE exists. Not creating from template. Delete it first if you want to recreate it."
else 
    echo "$SRC_FILE does not exist. Creating from template"
    cp src/main/scala/com/github/myyk/advent$1/bones.worksheet.sc $SRC_FILE
    sed -i '' "s/readInput(2022,1)/readInput($1,$2)/g" $SRC_FILE
fi
