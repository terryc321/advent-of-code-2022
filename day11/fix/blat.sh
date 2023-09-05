#!/bin/bash




sbcl --noinform --load "simple-state.lisp" --eval "(run  70 1 )" | tee dat/70-1.dat &
sleep 60
killall sbcl
sleep 10

sbcl --noinform --load "simple-state.lisp" --eval "(run  64 3 )" | tee dat/64-3.dat &
sleep 60
killall sbcl
sleep 10


sbcl --noinform --load "simple-state.lisp" --eval "(run  69 3 )" | tee dat/69-3.dat &

sleep 60
killall sbcl
sleep 10

sbcl --noinform --load "simple-state.lisp" --eval "(run  87 3 )" | tee dat/87-3.dat &

sleep 60
killall sbcl
sleep 10

sbcl --noinform --load "simple-state.lisp" --eval "(run  53 4 )" | tee dat/53-4.dat &
sleep 60
killall sbcl
sleep 10


sbcl --noinform --load "simple-state.lisp" --eval "(run  77 6 )" | tee dat/77-6.dat &

sleep 60
killall sbcl
sleep 10

sbcl --noinform --load "simple-state.lisp" --eval "(run  63 6 )" | tee dat/63-6.dat &

sleep 60
killall sbcl
sleep 10

sbcl --noinform --load "simple-state.lisp" --eval "(run  93 6 )" | tee dat/93-6.dat &

sleep 60
killall sbcl
sleep 10

sbcl --noinform --load "simple-state.lisp" --eval "(run  66 6 )" | tee dat/66-6.dat &

sleep 60
killall sbcl
sleep 10

sbcl --noinform --load "simple-state.lisp" --eval "(run  71 6 )" | tee dat/71-6.dat &

sleep 60
killall sbcl
sleep 10

sbcl --noinform --load "simple-state.lisp" --eval "(run  54 7 )" | tee dat/54-7.dat &

sleep 60
killall sbcl
sleep 10

sbcl --noinform --load "simple-state.lisp" --eval "(run  97 7 )" | tee dat/97-7.dat &

sleep 60
killall sbcl
sleep 10

sbcl --noinform --load "simple-state.lisp" --eval "(run  82 7 )" | tee dat/82-7.dat &

sleep 60
killall sbcl
sleep 10



