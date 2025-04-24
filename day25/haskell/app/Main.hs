module Main where

{-

tasks

[X] haskell - read file in lines
[ ] how do i convert number to string - show 5
[ ]

-}


{-

values used in strings are these

2 1 0 - =
2 1 0 -1 -2

decimal          snarf
        1              1
        2              2
        3             1=
        4             1-
        5             10
        6             11
        7             12
        8             2=
        9             2-
       10             20
       15            1=0
       20            1-0
     2022         1=11-2
    12345        1-0---0
314159265  1121-1110-1=0

 SNAFU  Decimal
1=-0-2     1747
 12111      906
  2=0=      198
    21       11
  2=01      201
   111       31
 20012     1257
   112       32
 1=-1=      353
  1-12      107
    12        7
    1=        3
   122       37


-}


-- convert string to decimal
-- string is a list in haskell
-- empty list is the decimal provided
-- index also power of 5 we are using p power , s sum

-- recursion scheme
convert2 :: String -> Integer -> Integer -> Integer
convert2 [] p s = s
convert2 ('2' : xs) p s = convert2 xs (p * 5) (s + (p * 2))
convert2 ('1' : xs) p s = convert2 xs (p * 5) (s + (p * 1))
convert2 ('0' : xs) p s = convert2 xs (p * 5) (s + (p * 0))
convert2 ('-' : xs) p s = convert2 xs (p * 5) (s + (p * (-1)))
convert2 ('=' : xs) p s = convert2 xs (p * 5) (s + (p * (-2)))
--
snafu :: String -> Integer
snafu x = convert2 (reverse x) 1 0

check = map snafu [ "1","2", "1=", "1-", "10", "11", "12", "2=", "2-", "20", "1=0", "1-0", "1=11-2", "1-0---0", "1121-1110-1=0"] 

check2 = map snafu ["1=-0-2","12111","2=0=","21","2=01","111","20012","112","1=-1=","1-12","12","1=","122"]
expect2 = [ 1747 , 906 ,198 , 11,201, 31,1257,32, 353,107, 7, 3, 37]

-- if we apply snafu to each string in input file and sum them up we get guess1
-- but what is that in terms of snafu 
guess = 32969743607087

-- find me a snafu for the guess provided
-- decimal to binary conversion

fives = 1 : (map (\x -> x * 5) fives)


foo :: Integer -> Integer -> [Integer]
foo n g = if n < 0 then []
          else let t = (5 ^ n) in
                 let r = g - t in
                   if r >= 0 then n : (foo n r)
                   else foo (n - 1) g
-- foo 20 guess
foo2 [] n = n
foo2 (x : xs) n = foo2 xs (n + (5 ^ x))
-- check if (snarf 5's version of )binary to decimal works 
-- foo2 (foo 20 guess) 0 

-- the result has to have a finite length
-- the result has to have one of the valid snafu digits 2 1 0 - =
-- just a case of trying everything for a finite length of string to find a match
-- possible we get multiple matches ? hypothesis...

-- we could incorporate trying

{-
we use a continuation what do after , we can do sequencing in a functional style
if too many steps then we abort and try a different letter

s1 xs step lim k ok =
  if step > lim then k []
  else let a = '2' : xs in
         let sa = snafu a in
           if sa == guess then ok a
           else s1 ('2' : xs) (step + 1) lim (\v -> s1 ('1' : xs) (step + 1) lim k ok) ok
-}


s1 :: String -> Integer -> Integer -> (String->String) -> (String -> String ) -> String
s1 xs step lim k ok =
  if step > lim then (k [])
  else let a = '2' : xs
           b = ('1' : xs)
           c = ('0' : xs)
           d = ('-' : xs)
           e = ('=' : xs) in
         let sa = snafu a
             sb = snafu b
             sc = snafu c
             sd = snafu d
             se = snafu e in
           if sa == guess then (ok a)
           else if sb == guess then (ok b)
           else if sc == guess then (ok c)
           else if sd == guess then (ok d)
           else if se == guess then (ok e)
           else s1 a (step + 1) lim (\v ->
                s1 b (step + 1) lim (\v ->
                s1 c (step + 1) lim (\v ->
                s1 d (step + 1) lim (\v ->
                s1 e (step + 1) lim (\v -> "") ok) ok) ok) ok) ok

      
-- -- 3 or 4 characters in length , try everything           
s02 lim = let xs = []
              step = 0 in
            s1 xs step lim (\v -> v) (\v -> v)

gee0 = [['2'],['1'],['0'],['-'],['=']]

-- construct a list of strings for all strings in list make a new list with 2 1 0 - = added to front
gee [] = []
gee (x : xs) = ('2' : x) : ('1' : x) : ('0' : x) : ('-' : x) : ('=' : x) : (gee xs)

ngee 0 x = x
ngee n x = ngee (n - 1) (gee x)

try1 = filter (\v -> v == guess) (map snafu (gee [['2'],['1'],['0'],['-'],['=']]))

--- 
try n = filter (\v -> snafu v == guess) (ngee n gee0)

-- try filter (\v -> v == guess) (map snafu (gee(gee(gee(gee(gee(gee [['2'],['1'],['0'],['-'],['=']])))))))
-- try filter (\v -> v == guess) (map snafu (gee(gee(gee(gee(gee(gee [['2'],['1'],['0'],['-'],['=']])))))))
-- try filter (\v -> v == guess) (map snafu (gee(gee(gee(gee(gee(gee [['2'],['1'],['0'],['-'],['=']])))))))
-- try filter (\v -> v == guess) (map snafu (gee(gee(gee(gee(gee(gee [['2'],['1'],['0'],['-'],['=']])))))))
-- try filter (\v -> v == guess) (map snafu (gee(gee(gee(gee(gee(gee [['2'],['1'],['0'],['-'],['=']])))))))
-- try filter (\v -> v == guess) (map snafu (gee(gee(gee(gee(gee(gee [['2'],['1'],['0'],['-'],['=']])))))))

-- try 12  ties up computer ... we may well be substantially larger in our guess

{-
say we know first of an infinite long solution will be zeros ... something non zero , ignore zero as first
candidate
"=2222222" is this positive ?
"2=======" is this positive ? which is higher ?

good guess so far
snafu "2-=1-==============="

what is the guess in terms of modulo 5 ?
is it a composite of various

let a = "2====================" in (snafu a , length a)
(143051147460938,21)
21 characters in length total , can it be more ?? no because each character has to be 2 1 0 - =
since = is most negative can go if first digit is 2 then

guess % 5 

split number into remainder and divisor

              2
              1
  ----------- 0
              -
              =
  in 3nd place this mean difference of 100

(2,-2)
(10,-10)
(50,-50)
(250,-250)

-- fx 0 = []
-- fx n = let g = n `mod` 5 in g : (fx ((n - g) `div` 5))
-- fy n = reverse (fx n)

-- g = guess
-- a1 = g `mod` 5
-- b = (g - a1) `div` 5
-- b1 = b `mod` 5 

-}

f xs = let sa = snafu xs in (xs, "greater : " ++ (show (sa > guess)) , sa - guess)

{-
--- given a best guess for "THE-STRING" try changing a letter to see if

sounds like guessing again

"=" "-" "0" "1" "2"

before (our-choice) after ---> to make a full string which we snafu and pick closest choice ?
narrowing initially before = [] choice after = list of zeros

-}

--- where cmp is <= 
qs [] cmp = []
qs (x:xs) cmp = let a = qs [y | y <- xs, cmp y x] cmp
                    b = [x]
                    c = qs [y | y <- xs, not (cmp y x)] cmp
                in a ++ b ++ c

-- sort values
qsTest = qs [1,2,3,4] (\v1 -> \v2 -> v1 <= v2)
qsTest2 = qs [1,2,3,4] (\v1 -> \v2 -> v1 >= v2)

-- sort by 1st element of tuple 
qsTest3 = qs [(1,"a"),(2,"b"),(3,"c")] (\(v1,_) -> \(v2,_) -> v1 <= v2)
qsTest4 = qs [(1,"a"),(2,"b"),(3,"c")] (\(v1,_) -> \(v2,_) -> v1 >= v2)

--tupleTest x y = \(v1,_) -> \(v2,_) -> (v1 >= v2)
tupleTest (v1,_) (v2,_) = (abs (v1 - guess)) <= (abs (v2 - guess))

qsTest5 = qs [(1,"a"),(2,"b"),(3,"c")] tupleTest
qsTest6 = qs [(3,"c"),(2,"b"),(1,"a")] tupleTest


ff :: String -> String -> [(Integer,String)]
ff before after = let sa = before ++ "2"
                      sb = before ++ "1"
                      sc = before ++ "0"
                      sd = before ++ "-"
                      se = before ++ "=" in
                     let a = before ++ sa ++ after
                         b = before ++ sb ++ after
                         c = before ++ sc ++ after
                         d = before ++ sd ++ after
                         e = before ++ se ++ after in
                        let vas = map (\v -> (snafu v, v)) [a,b,c,d,e] in
                           let sorted = qs vas tupleTest in
                             [head sorted]


ff2 :: String -> String -> String
ff2 before [] = before
ff2 before after = let sa = before ++ "2"
                       sb = before ++ "1"
                       sc = before ++ "0"
                       sd = before ++ "-"
                       se = before ++ "=" in
                     let a = before ++ sa ++ after
                         b = before ++ sb ++ after
                         c = before ++ sc ++ after
                         d = before ++ sd ++ after
                         e = before ++ se ++ after in
                        let vas = map (\v -> (snafu v, v)) [a,b,c,d,e] in
                           let sorted = qs vas tupleTest in
                             let (sv,v) = head sorted
                                 (af : afr) = after in          
                               ff2 (before ++ v) afr
                             
                             

-- f "20000000000000000000"
-- ("20000000000000000000","greater : True",5177229049163)

-- from our experiments    
f1 = let before = ""
         after = "0000000000000000000"
     in ff2 before after

        


{-
-}


                                           

                         
                     
                     





            

main :: IO ()
main = do putStrLn "Hello, Haskell!"
          contents <- readFile "../input.txt"
          let linesOfFile = lines contents
          let values = map snafu linesOfFile
          putStrLn ("the values " ++ (show values))
          let total = foldr (+) 0 values
          putStrLn ("the total = " ++ (show total))
          --mapM_ putStrLn (show values)
