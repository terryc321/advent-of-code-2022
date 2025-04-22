
-- how can make documentation from this file ?
-- advent of code 2022 day 19 
--
-- Blueprint 1:
--   Each ore robot costs 4 ore.
--   Each clay robot costs 2 ore.
--   Each obsidian robot costs 3 ore and 14 clay.
--   Each geode robot costs 2 ore and 7 obsidian.
--
-- Blueprint 2:
--   Each ore robot costs 2 ore.
--   Each clay robot costs 3 ore.
--   Each obsidian robot costs 3 ore and 8 clay.
--   Each geode robot costs 3 ore and 12 obsidian.
--  


-- > idris2
-- > :l "fun.idr"
-- > :c fun main       compile file fun with entry point main i guess
--  generates build/exec/fun written 
-- > idris2 fun.idr
-- > :t isAllowed      shows type of isAllowed function
-- Main.isAllowed : Char -> Bool
--
-- how can we generate interactive book ?
-- interactive documentation

-- with emacs integration from idris2-mode see github
-- M-x idris2-repl
-- we get a idris2 repl
-- cd docs 
-- make docs 
-- generates a pdf file can read 

-- emacs C-c C-l 
-- idris load file 
-- everything gets fixed

module Main


-- Vect
import Data.Vect
-- :browse Data.Vect
-- :browse Prelude

-- no such import
-- import Data.Vect.Extra  -- for helpers like `replicate`, `take`, etc.

two : Int
two = 2

-- > Main.pi 
-- 3.14
pi : Double
pi = 3.14 

-- character
zed : Char
zed = 'z'

-- equality over character
zedIsZ = 'z' == zed

quux : Bool
quux = False

-- no declaration here , but 
quux2 = True

-- Allowed characters
-- > isAllowed '0'      is the character zero allowed ? Yes it is 
-- True
isAllowed : Char -> Bool
isAllowed c = c == '0' || c == '1' || c == 'X'

-- foo 94 gives yes  ......  64 + 30  is 94
foo : Int -> String
foo x = if x == 8 * 8 + 30 then "Yes!" else "No!"


-- A type-level predicate: all characters must be allowed
AllAllowed : Vect n Char -> Type
AllAllowed [] = ()
AllAllowed (x :: xs) = (isAllowed x = True, AllAllowed xs)

-- The main type: 36 characters, all allowed
AllowedMask : Type
AllowedMask = (xs : Vect 36 Char ** AllAllowed xs)

validChars : Vect 36 Char
validChars =  ['0', '1', 'X', '0', '1', '1', '0', '1', 'X', '1', '0', '0',
              '1', '0', '1', '1', 'X', '1', '0', '0', '1', '0', '1', 'X',
              '1', '0', '0', '1', '0', '1', '1', '0', '1', '1', '0', '1']
              
listIsString : Bool
listIsString = ['0', '1', 'X', '0', '1', '1', '0', '1', 'X', '1', '0', '0',
              '1', '0', '1', '1', 'X', '1', '0', '0', '1', '0', '1', 'X',
              '1', '0', '0', '1', '0', '1', '1', '0', '1', '1', '0', '1'] == unpack "01X01101X1001011X100101X100101101101"
                            
                            


-- IsAllowedMask : Vect 36 Char -> Type
-- IsAllowedMask = All isAllowed


-- IsAllowedMask = All (x => isAllowed x = True)

-- validMask : AllowedMask
-- validMask with (isAllowed validChars)
--   validMask | Yes prf = (validChars ** prf)
--   validMask | No _ = absurd ?impossible
  
-- this cannot be proved below , as said by chatgpt
-- validMask : AllowedMask
-- validMask = (the (Vect 36 Char) 
--   ['0', '1', 'X', '0', '1', '1', '0', '1', 'X', '1', '0', '0',
--    '1', '0', '1', '1', 'X', '1', '0', '0', '1', '0', '1', 'X',
--    '1', '0', '0', '1', '0', '1', '1', '0', '1', '1', '0', '1'] ** ?p)

-- validMask : AllowedMask
-- validMask = (the (Vect 36 Char) 
--   ['0', '1', 'X', '0', '1', '1', '0', '1', 'X', '1', '0', '0',
--    '1', '0', '1', '1', 'X', '1', '0', '0', '1', '0', '1', 'X',
--    '1', '0', '0', '1', '0', '1', '1', '0', '1', '1', '0', '1'] Refl)



-- deliberately make a hole but putting putStrLn ?greeting
-- the hole ?greeting     means something goes here but i just havent got to it yet
-- 
main : IO ()
main = putStrLn "solving day 19 of advent-code 2022"








