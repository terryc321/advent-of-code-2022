#!/bin/bash

echo 
echo computing 71 - 0
sbcl --noinform --load "super-simple.lisp" --eval "(run  71 0 )"  | tee dat/71-0.dat & 
echo 
echo 
sleep 60
killall sbcl
sleep 10

echo 
echo computing 56 - 0
sbcl --noinform --load "super-simple.lisp" --eval "(run  56 0 )"  | tee dat/56-0.dat & 
echo 
echo 
sleep 60
killall sbcl
sleep 10

echo 
echo computing 50 - 0
sbcl --noinform --load "super-simple.lisp" --eval "(run  50 0 )"  | tee dat/50-0.dat & 
echo 
echo 
sleep 60
killall sbcl
sleep 10

echo 
echo computing 73 - 0
sbcl --noinform --load "super-simple.lisp" --eval "(run  73 0 )"  | tee dat/73-0.dat & 
echo 
echo 
sleep 60
killall sbcl
sleep 10

echo 
echo computing 70 - 1
sbcl --noinform --load "super-simple.lisp" --eval "(run  70 1 )"  | tee dat/70-1.dat & 
echo 
echo 
sleep 60
killall sbcl
sleep 10

echo 
echo computing 89 - 1
sbcl --noinform --load "super-simple.lisp" --eval "(run  89 1 )"  | tee dat/89-1.dat & 
echo 
echo 
sleep 60
killall sbcl
sleep 10

echo 
echo computing 82 - 1
sbcl --noinform --load "super-simple.lisp" --eval "(run  82 1 )"  | tee dat/82-1.dat & 
echo 
echo 
sleep 60
killall sbcl
sleep 10

echo 
echo computing 52 - 2
sbcl --noinform --load "super-simple.lisp" --eval "(run  52 2 )"  | tee dat/52-2.dat & 
echo 
echo 
sleep 60
killall sbcl
sleep 10

echo 
echo computing 95 - 2
sbcl --noinform --load "super-simple.lisp" --eval "(run  95 2 )"  | tee dat/95-2.dat & 
echo 
echo 
sleep 60
killall sbcl
sleep 10

echo 
echo computing 94 - 3
sbcl --noinform --load "super-simple.lisp" --eval "(run  94 3 )"  | tee dat/94-3.dat & 
echo 
echo 
sleep 60
killall sbcl
sleep 10

echo 
echo computing 64 - 3
sbcl --noinform --load "super-simple.lisp" --eval "(run  64 3 )"  | tee dat/64-3.dat & 
echo 
echo 
sleep 60
killall sbcl
sleep 10

echo 
echo computing 69 - 3
sbcl --noinform --load "super-simple.lisp" --eval "(run  69 3 )"  | tee dat/69-3.dat & 
echo 
echo 
sleep 60
killall sbcl
sleep 10

echo 
echo computing 87 - 3
sbcl --noinform --load "super-simple.lisp" --eval "(run  87 3 )"  | tee dat/87-3.dat & 
echo 
echo 
sleep 60
killall sbcl
sleep 10

echo 
echo computing 70 - 3
sbcl --noinform --load "super-simple.lisp" --eval "(run  70 3 )"  | tee dat/70-3.dat & 
echo 
echo 
sleep 60
killall sbcl
sleep 10

echo 
echo computing 98 - 4
sbcl --noinform --load "super-simple.lisp" --eval "(run  98 4 )"  | tee dat/98-4.dat & 
echo 
echo 
sleep 60
killall sbcl
sleep 10

echo 
echo computing 72 - 4
sbcl --noinform --load "super-simple.lisp" --eval "(run  72 4 )"  | tee dat/72-4.dat & 
echo 
echo 
sleep 60
killall sbcl
sleep 10

echo 
echo computing 98 - 4
sbcl --noinform --load "super-simple.lisp" --eval "(run  98 4 )"  | tee dat/98-4.dat & 
echo 
echo 
sleep 60
killall sbcl
sleep 10

echo 
echo computing 53 - 4
sbcl --noinform --load "super-simple.lisp" --eval "(run  53 4 )"  | tee dat/53-4.dat & 
echo 
echo 
sleep 60
killall sbcl
sleep 10

echo 
echo computing 97 - 4
sbcl --noinform --load "super-simple.lisp" --eval "(run  97 4 )"  | tee dat/97-4.dat & 
echo 
echo 
sleep 60
killall sbcl
sleep 10

echo 
echo computing 51 - 4
sbcl --noinform --load "super-simple.lisp" --eval "(run  51 4 )"  | tee dat/51-4.dat & 
echo 
echo 
sleep 60
killall sbcl
sleep 10

echo 
echo computing 79 - 5
sbcl --noinform --load "super-simple.lisp" --eval "(run  79 5 )"  | tee dat/79-5.dat & 
echo 
echo 
sleep 60
killall sbcl
sleep 10

echo 
echo computing 77 - 6
sbcl --noinform --load "super-simple.lisp" --eval "(run  77 6 )"  | tee dat/77-6.dat & 
echo 
echo 
sleep 60
killall sbcl
sleep 10

echo 
echo computing 55 - 6
sbcl --noinform --load "super-simple.lisp" --eval "(run  55 6 )"  | tee dat/55-6.dat & 
echo 
echo 
sleep 60
killall sbcl
sleep 10

echo 
echo computing 63 - 6
sbcl --noinform --load "super-simple.lisp" --eval "(run  63 6 )"  | tee dat/63-6.dat & 
echo 
echo 
sleep 60
killall sbcl
sleep 10

echo 
echo computing 93 - 6
sbcl --noinform --load "super-simple.lisp" --eval "(run  93 6 )"  | tee dat/93-6.dat & 
echo 
echo 
sleep 60
killall sbcl
sleep 10

echo 
echo computing 66 - 6
sbcl --noinform --load "super-simple.lisp" --eval "(run  66 6 )"  | tee dat/66-6.dat & 
echo 
echo 
sleep 60
killall sbcl
sleep 10

echo 
echo computing 90 - 6
sbcl --noinform --load "super-simple.lisp" --eval "(run  90 6 )"  | tee dat/90-6.dat & 
echo 
echo 
sleep 60
killall sbcl
sleep 10

echo 
echo computing 88 - 6
sbcl --noinform --load "super-simple.lisp" --eval "(run  88 6 )"  | tee dat/88-6.dat & 
echo 
echo 
sleep 60
killall sbcl
sleep 10

echo 
echo computing 71 - 6
sbcl --noinform --load "super-simple.lisp" --eval "(run  71 6 )"  | tee dat/71-6.dat & 
echo 
echo 
sleep 60
killall sbcl
sleep 10

echo 
echo computing 54 - 7
sbcl --noinform --load "super-simple.lisp" --eval "(run  54 7 )"  | tee dat/54-7.dat & 
echo 
echo 
sleep 60
killall sbcl
sleep 10

echo 
echo computing 97 - 7
sbcl --noinform --load "super-simple.lisp" --eval "(run  97 7 )"  | tee dat/97-7.dat & 
echo 
echo 
sleep 60
killall sbcl
sleep 10

echo 
echo computing 87 - 7
sbcl --noinform --load "super-simple.lisp" --eval "(run  87 7 )"  | tee dat/87-7.dat & 
echo 
echo 
sleep 60
killall sbcl
sleep 10

echo 
echo computing 70 - 7
sbcl --noinform --load "super-simple.lisp" --eval "(run  70 7 )"  | tee dat/70-7.dat & 
echo 
echo 
sleep 60
killall sbcl
sleep 10

echo 
echo computing 59 - 7
sbcl --noinform --load "super-simple.lisp" --eval "(run  59 7 )"  | tee dat/59-7.dat & 
echo 
echo 
sleep 60
killall sbcl
sleep 10

echo 
echo computing 82 - 7
sbcl --noinform --load "super-simple.lisp" --eval "(run  82 7 )"  | tee dat/82-7.dat & 
echo 
echo 
sleep 60
killall sbcl
sleep 10

echo 
echo computing 59 - 7
sbcl --noinform --load "super-simple.lisp" --eval "(run  59 7 )"  | tee dat/59-7.dat & 
echo 
echo 
sleep 60
killall sbcl
sleep 10
