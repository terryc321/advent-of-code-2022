# aoc22day23

- [X] Open file for reading
- [X] Read line by line
- [X] Verify input output
- [X] Elf structures position x y proposed x y
- [X] Oriented north +Y east +X
- [ ] Simple solution first

## Simple solution first

get working code even if it is very slow

## Orientation

chosen +Y as North and +X as East so more like a math axis applied to a 2d map

## Verify input output

parse input and produce output . run a diff against both . should be same

## Multidimensional arrays

can we not have arrays from 1 to n inclusive and array dimensions report size of
array n by n 

## Iterate over a string

instead of looping using an index from 0 to len - 1 then taking char of string instead
coerce the string to a list and use loop for in  

- [X] No potential to char wrong index
- [X] Less code

```lisp
	(loop for x from 0 to (- len 1) do
	  (let ((ch (char line x)))
```

```lisp
	(loop for ch in (coerce line 'list) do
```

