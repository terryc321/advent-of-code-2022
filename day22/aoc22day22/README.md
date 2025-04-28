# aoc22day22

- [X] Sanity input output and parsing done correctly
 kdiff3 binary equivalence confirmed
- [ ] Puzzle requires knowing the wrap around next position 
  lets pre compute this 
```lisp

# is a gate cannot go here
. is open square 
' ' empty space which is no mans land cannot go here either 

structure 
x 
y 
left -> take to another structure ? which represents a square on the grid
right ->
up ->
down ->
```

- [ ] Understand testing framework fiveam
- [ ] Integrate fiveam into a quickproject (here aoc22day22)

### quickproject 

fresh project with fiveam a testing framework.
how can we incorporate unit testing into the project ?

```lisp
(quickproject:make-project #P"aoc22day22" :depends-on '(fiveam))
```

### fiveam testing

run all tests using

```lisp
(aoc22day22-tests:run!)
```

or whenever we compile tests , they get run 
```lisp
(setf fiveam:*run-test-when-defined* t)
```

