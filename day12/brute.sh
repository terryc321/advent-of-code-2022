#!/bin/bash

clear

# pipe data out to visualiser
sbcl --noinform --load "fun.lisp" --eval "(aoc22::solve (aoc22::board))" | ./xwin




