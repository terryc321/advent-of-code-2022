;;;; aoc22day22-tests.lisp

(in-package :aoc22day22-tests)

;; when file is compiled - tests get run
(setf fiveam:*run-test-when-defined* t)



#|

Testing with FiveAM FiveAM has 3 levels of abstraction: check, test
and suite. As you may have guessed:

- (1) A check is a single assertion that checks that its argument is truthy.

The most used check is is. For example, (is (= 2 (+ 1 1))).  A
test is the smallest runnable unit.

- (2) A test case may contain multiple checks.

Any check failure leads to the failure of the whole test.

- (3) A suite : is a collection of tests.

When a suite is run, all tests inside would be performed.

A suite allows paternity, which means that running a suite will run all the tests
defined in it and in its children suites.

|#

(test test-demo  "This demonstrates the basic use of test and check."
  (is (listp (list 1 2)))
  (is (= 5 (+ 2 3)) "This should pass.")  ; &rest reason-args
  (is (= 4 4.1) "~D and ~D are not = to each other." 4 4.1))

(test test-twice  "aoc22day22::twice"
  (let ((a (aoc22day22::twice 3)))
    (is (= 6 a) "~a and ~a are not = to each other." 6 a)))

(test test-twice  "aoc22day22::fib"
  (let ((a (aoc22day22::fib 10))
	(b 55))
    (is (= a b) "~a and ~a are not = to each other." a b)))

(test test-twice  "aoc22day22::fac"
  (let ((a (aoc22day22::fac 5))
	(b 120))
    (is (= a b) "~a and ~a are not = to each other." a b)))




