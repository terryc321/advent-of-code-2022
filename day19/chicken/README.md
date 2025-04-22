

continuation.scm

made use of call/cc 
use a vector around 40 items , only use 1 through 30 inclusive
make each entry of the vector a continuation
 initially this continuation is simply (lambda (v) (blue1-run))
  so discard value v passed in , runs blue1-run

each of the 30 procedures is created from regex on the string
"Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 4 ore. Each obsidian robot costs 4 ore and 9 clay. Each geode robot costs 3 ore and 9 obsidian."

each blueprint has a blueprint-no number , ore , clay , obsidian , geode all exactly same structure just values change


