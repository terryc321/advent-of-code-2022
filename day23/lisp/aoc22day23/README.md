# aoc22day23
### _Your Name <your.name@example.com>_

This is a project to do ... something.

## License

Specify license here

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

