#!/bin/bash

curl https://adventofcode.com/$1/day/$2/input --cookie "session=$ADVENT_SESSION" --create-dirs --output ../samples/$1/day$2.txt
