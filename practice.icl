module practice
import StdEnv

// 1. Using recursive function generate a list with elements from 1 to x,
// x is function parameter (a dot-dot expression is not good).

listGen :: Int -> [Int]
listGen 0 = []
listGen x = listGen (x-1) ++ [x]

//Start = listGen 10

// 2. Compute the sum of each sublist.
// [[1,2], [3,4,5], [6,5,9,7], [], [8]] -> [3,12,27,0,8]

sums :: [[Int]] -> [Int]
sums x = map sum x

//Start = sums [[1,2], [3,4,5], [6,5,9,7], [], [8]]

// 3. Insert x as first element in every sublist of a list.
// if the sublist was empty then x will be the only element in the new sublist.
// [[1,2], [3,4,5], [6,5,9,7], [], [8]] 100 -> [[100,1,2], [100,3,4,5], [100,6,5,9,7], [100], [100,8]]

insx :: [[Int]] Int -> [[Int]]
insx [] a = []
insx [x:xs] a = [[a]++x] ++ insx xs a

//Start = insx  [[1,2], [3,4,5], [6,5,9,7], [], [8]] 100

/* 1.
Delete the middle eltement of each list (if length is odd) in the list.
e.g. [[1,2,3],[3],[4,5,6,7],[],[0,1,6,3,5]] -> [[1,3],[],[4,5,6,7],[],[0,1,3,5]]
*/

f1::[[Int]]->[[Int]]
f1 [] = []
f1 [x:xs]
|isEven (length x) = [x] ++ f1 xs
= [removeAt ((length x)/2) x] ++ f1 xs

//|isEven(length x) = x
//=map removeAt 

//Start = f1 [[1,2,3],[3],[4,5,6,7],[],[0,1,6,3,5]]  //[[1,3],[],[4,5,6,7],[],[0,1,3,5]]
//Start = f1 [[1,2,6,8,3],[9,3],[0,5,0,6,7,0,0],[],[0,1,0,6,3,5]] //[[1,2,8,3],[9,3],[0,5,0,7,0,0],[],[0,1,0,6,3,5]]
//Start = f1 [[0],[3,6],[4,5,6],[],[0,1,6,9,7,3,5]]  //[[],[3,6],[4,6],[],[0,1,6,7,3,5]]

/* 2.
Delete the third eltement of each list in the list.
e.g. [[1,2,3],[3],[4,5,6,7],[],[0,1,6,3,5]] -> [[1,2],[3],[4,5,7],[],[0,1,3,5]]
*/

f2::[[Int]]->[[Int]]
f2 [] = []
f2 [x:xs]
|(length x) < 3 = [x] ++ f2 xs
= [removeAt 2 x] ++ f2 xs


//Start = f2 [[1,2,3],[3],[4,5,6,7],[],[0,1,6,3,5]]  //[[1,2],[3],[4,5,7],[],[0,1,3,5]]
//Start = f2 [[1,2,6,8,3],[9,3],[0,5,0,6,7],[],[0,1,6,3,5,8]]  //[[1,2,8,3],[9,3],[0,5,6,7],[],[0,1,3,5,8]]
//Start = f2 [[0],[3],[4,5,6],[],[0,1,6,9,7,3,5]]  //[[0],[3],[4,5],[],[0,1,9,7,3,5]]

/* 3.
Move the elements that are odd and less than 7 to the front of the list.
*/

f3 :: [Int] -> [Int]
f3 [] = []
f3 [x:xs] = filter works [x:xs] ++ filter notw [x:xs]

notw :: Int -> Bool
notw x = not (works x)

works :: Int -> Bool
works x
|isOdd x && x<7 = True
=False

//Start = works 5
//Start = aux [1..10]

//Start = f3 [1..10] //[1,3,5,2,4,6,7,8,9,10]
//Start = f3 [15,13..1] //[5,3,1,15,13,11,9,7]
//Start = f3 [12321 ,1, 4, 34, 7, 9, 525] //[1,12321,4,34,7,9,525]

/*
Keep the odd elements in the first list and the even elements in  the second list,
the odd in the third list and the even in  the second list, and so on..
*/

nikola::[[Int]] Int->[[Int]]
nikola [] _=[]
nikola [x:xs] n
|isOdd n=[filter isOdd x]++nikola xs (n+1)
|isEven n=[filter isEven x]++nikola xs (n+1)
f4::[[Int]] ->[[Int]]
f4 []=[]
f4 [x:xs]=nikola [x:xs] 1

//Start = f4 [[1,2,3],[2,3,4,5],[],[4,4,5,5,6],[1]] //[[1,3],[2,4],[],[4,4,6],[1]]
//Start = f4 [[1,0,1],[2,1,0,3,4,5],[1],[4,4,0,0,5,7,5,6],[1,2]] //[[1,1],[2,0,4],[1],[4,4,0,0,6],[1]]

/*
use map to insert n in the middle of every sublist of a list.
if there is one element in the middle of sublist, insert n before it 
e.g. if n = 4 , [1,2,3] -> [1,4,2,3] (insert 4 before 2)
*/

f5::Int [[Int]] ->[[Int]]
f5 n [] = [ [n] ]
f5 n [x:xs] = map (aux n) [x:xs]


aux:: Int [Int] -> [Int]
aux n x = insertAt ((length x)/2) n x

//Start = f5 2 [[],[1],[1,3],[3,4,5]] //[[2],[2,1],[1,2,3],[3,2,4,5]]
//Start = f5 8 [[0,1],[1],[1,6,5,3],[3,4,5,7,0],[]]//[[0,8,1],[8,1],[1,6,8,5,3],[3,4,8,5,7,0],[8]]

// Given two integers, write a function
// that will give us their least common multiple.
f6 :: Int Int -> Int
f6 a b =( abs(a * b) / gcd (abs a) (abs b) )
//Start = f6 3 4 //12
//Start = f6 0 5 //0
//Start = f6 -7 4 //28
//Start = f6 12 10 //60
//Start = f6 0 4

// Given 'n' number of friends
// and 'm' pieces of cake, 
// how many different ways are there to
// distribute these pieces of cake?
fac :: Int -> Int
fac x
|x==0 = 1
=x * fac (x-1)

f7 :: Int Int -> Int
f7 n k 
|n<=0 || k<=0 = 0
=(fac (abs (n))) / ((fac (abs (k)))*(fac (abs (n-k))))
//n! / (n-k)!

//Start = f7 5 2 //10
//Start = f7 5 8 //56
//Start = f7 5 -13 //0
//Start = f7 -4 9999 //0


// Given an integer, write a function
// that will check if each digit is even.
f8 :: Int -> Bool
f8 x
|x == 0 = True
|x rem 2 == 0 = f8 (x/10)
=False

//Start = f8 1234 //False
//Start = f8 506 //True
//Start = f8 -846 //True

// Given an integer, write a function
// that returns the sum of all
// odd numbers less than it.
p :: Int -> [Int]
p 0 =[]
p x = [x rem 10 : p (x/10)]
//Start = p 123

f9 :: Int -> Int
f9 x
|x<=0=0
|isOdd x = (x-2)+(x-4)+(x-6)
=(x-1)+(x-3)+(x-5)+(x-7)+(x-9)+(x-11)

//Start = f9 5 //4
//Start = f9 12 //36
//Start = f9 -6 //0

/*
p :: Int -> [Int]
p 0 =[]
p x = [x rem 10 : p (x/10)]
*/


// How many different ways are there
// to distribute 'n' homework problems
// amongst 'm' number of friends?
// f2 :: Int Int -> Int
// Start = f2 200 4 //64684950
// Start = f2 2 4 //6
// Start = f2 ~4 4 //0
// Start = f2 9999999 0 //0

// Given an integer, write a function
// that will sum up its digits.
f12 :: Int -> Int
f12 x = sum (p (x))

//Start = f12 1234 //10
//Start = f12 506 //11
//Start = f12 6 //6
//Start = f12 -91 //10

// Determine the greatest common divisor of two
// positive integer numbers.
f10 :: Int Int -> Int
f10 x y
|x==y = x
|x>y = f10 (x-y) y
|y>x = f10 x (y-x)

//Start = f10 5 15 // 5
//Start = f10 6 11 // 1
//Start = f10 6 15 // 3

// You are climbing a stair case. It takes n steps to reach to the top.
// Each time you can either climb 1 or 2 steps.
// In how many distinct ways can you climb to the top?
climbStairs n = climb_stairs 0 n

climb_stairs i n
| i > n = 0
| i == n = 1
= (climb_stairs (i+1) n) + (climb_stairs (i+2) n)

//Start = climbStairs 5


//Start = climbStairs 2 // 2 (1+1,2)
//Start = climbStairs 3 // 3 (1+1+1,1+2,2+1)

// Given an integer, write a function that returns
// the largest digit in the integer.
f13 :: Int -> Int
f13 a
|a<0 = f13 (abs a)
|a<10 = a
=f132 (a/10) (a rem 10)

f132 a b
|a<=0 = b
|(a rem 10)>b = f132(a/10) (a rem 10)
|(a rem 10)<b = f132 (a/10) b
//Start = f132 51 49
//Start = 54 rem 10

//Start = f13 564 //6
//Start = f13 5 //5
//Start = f13 -6793 //9

/**
  * Write a function that tests the Collatz conjecture.
  * Given a number 'n', if it is even, divide it by 2.
  * If 'n' is odd, multiply by 3 and add 1.
  * Repeat until the number reaches 1.
  * This function should return the total stopping time,
  * which is the number of steps it took to reach 1.
  *
  * Total: 30pts
  */
  
collatzz :: Int Int -> Int
collatzz x cnt
|x==1=cnt
|isEven x = collatzz (x/2) (cnt+1)
=collatzz ((x*3)+1) (cnt+1)

collatz :: Int -> Int
collatz n
|n<=0=0
=collatzz n 0

//Start = collatz 34 //13
//Start = collatz(collatz 10000001) //33
//Start = collatz 1 //0
//Start = collatz (~56) //0

/**
  * Write a function that takes an integer 'x'
  * and returns a boolean corresponding to whether or not x is prime.
  * A prime number is a number that has only itself and 1 as divisors.
  * 1 is not prime.
  *
  * Total: 30pts
  */
isPrime :: Int -> Bool
isPrime x
|x<=1 = False
=prajm x 2

prajm :: Int Int -> Bool
prajm a b
|a==b=True
|a rem b <> 0 = prajm a (b+1)
=False

//Start = isPrime 5 //True
//Start = isPrime (~3) //False (Negative numbers don't count, only Natural numbers)
//Start = isPrime 0 //False
//Start = isPrime 1 //False
//Start = isPrime 28736 //False
//Start = isPrime 11

/**
  * Write a function that takes an integer 'x'
  * and checks if this number is a palindrome.
  * A palindrome is a number or word that is identical 
  * when written forward or backwards.
  *
  * e.g. 1234 is not a palindrome. 145626541 is a palindrome.
  * Total: 40pts
  */
isPalindrome :: Int -> Bool
isPalindrome x
|x<0 = False
|p x == reverse (p x) = True
=False

//Start = isPalindrome 0 //True
//Start = isPalindrome 55 //True
//Start = isPalindrome 49594 //True
//Start = isPalindrome 1337 //False
//Start = isPalindrome (~57975) //False

// 1. Compute the sum of the list excluding the largest and the smallest number
idk :: [Int] -> Int
idk []=0
idk [x]=0
idk x = ((sum (init (sort x))) - hd (sort x))

//Start = idk [1..5] //  9
//Start = idk [4,6,2,7,1,9] //  19
//Start = idk [-4,6,2,7,1,9] // 16

// 2. Replace every second number from the list by 1,2,3...
replacee :: [Int] -> [Int]
replacee list = rreplace list 1

rreplace :: [Int] Int -> [Int]
rreplace [] n = []
rreplace [x] n = [x]
rreplace [x,y:xs] n = [x,n : rreplace xs (n+1)] 

//Start = replacee [] //[]
//Start = replacee [4] //[4]
//Start = replacee [6,4] //[6,1]
//Start = replacee [6,6,6,6] //[6,1,6,2]
//Start = replacee [1,9,0,2,4,7,0,5,1,8,3,0,1,2] // [1,1,0,2,4,3,0,4,1,5,3,6,1,7]

// 3. Divide a list of integer into two lists
// One containing even numbers and one containing odd numbers
//[1,2,3,4,5,6] -> [[2,4,6],[1,3,5]]
divide :: [Int] -> [[Int]]
divide list = [filter isEven list, filter isOdd list]

//Start = divide [1..6] //[[2,4,6],[1,3,5]]
//Start = divide [34..43]  //[[34,36,38,40,42],[35,37,39,41,43]]

/**
  * Write a function that takes a number 'n'
  * and returns a list of the first n Fibonacci numbers.
  * A Fibonacci number is a sequence where F_0 = 0, F_1 = 1, and F_n = F_(n-2) + F_(n-1)
  * For example, FibList 8 = [1,1,2,3,5,8,13,21]
  *
  * Note: You must use recursion.
  *
  * Total: 20pts
  */
  
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
//Start = fib 8

FibList :: Int -> [Int]
FibList n
|n<=0=[]
=FibList (n-1) ++ [fib n]

//Start = FibList 8 //[1,1,2,3,5,8,13,21]
//Start = FibList -3 //[]
//Start = FibList 0 //[]

/**
  * Write a function that takes a list of coefficients for a polynomial
  * and evaluates it at an integer given as the second parameter.
  * A list such as [1,6,9] would represent the the polynomial x^2+6x+9.
  * Note: Exponentiation via (^) or a custom exponential function
  * is NOT allowed.
  * 
  * For example: Evaluate [1,6,9] 2 = 25
  * Hint: Use Horner's Method
  * e.g. 3x^2 + 2x -4 = -4 + 2x + 3x^2 = -4 + x(2 + x(3)))
  *
  * Total: 50pts
  */
Evaluate :: [Int] Int -> Int
Evaluate [] _ = 0
Evaluate [x] _ = x
Evaluate [x:xs] a = ((last [x:xs]) + (a*(Evaluate (init [x:xs]) a)))

//Start = Evaluate [1,6,9] 2 //25
//Start = Evaluate [1337] 12345 //1337
//Start = Evaluate [] 9001 //0
//Start = Evaluate [243,810,1080,720,240,32] (~2) //-1024

/**
  * Write a function that takes two lists of Strings,
  * one containing First Names and the other containing
  * Family Names, and creates a list where each sublist
  * will contain the First Names matched with the 
  * Family Names.
  * In the case that the list of Family Names has only
  * one Family name, assign it to every Frist Name.
  */
MakeFamily :: [String] [String] -> [[String]]
MakeFamily [] _ = []
MakeFamily _ [] = []
MakeFamily [x:xs] [a] = [[x,a]] ++ MakeFamily xs [a]
MakeFamily [x:xs] [a:as] = [[x,a]] ++ MakeFamily xs as

//Start = MakeFamily ["Hossameldin", "Zuka", "Nicola", "Tringa", "Ying", "Nghia", "Pedro", "Evan"] ["Abdin", "Tsinadze", "Cenic", "Sylaj", "Xue", "Le", "Figueiredo", "Sitt"] //[["Hossameldin","Abdin"],["Zuka","Tsinadze"],["Nicola","Cenic"],["Tringa","Sylaj"],["Ying","Xue"],["Nghia","Le"],["Pedro","Figueiredo"],["Evan","Sitt"]]
//Start = MakeFamily ["Hossameldin", "Zuka", "Nicola", "Tringa", "Ying", "Nghia", "Pedro", "Evan"] ["Zsok"] //[["Hossameldin","Zsok"],["Zuka","Zsok"],["Nicola","Zsok"],["Tringa","Zsok"],["Ying","Zsok"],["Nghia","Zsok"],["Pedro","Zsok"],["Evan","Zsok"]]
//Start = MakeFamily [] ["Abdin", "Tsinadze", "Cenic", "Sylaj", "Xue", "Le Minh", "Figueiredo", "Sitt"] //[]
//Start = MakeFamily ["Hossameldin", "Zuka", "Nicola", "Tringa", "Ying", "Nghia", "Pedro", "Evan"] [] //[]

// Reverse the list recurievely
rev :: [Int] -> [Int]
rev [] = []
rev list = [(last list) : rev (init list)]

//Start = rev [] // []
//Start = rev [1,2,3,4,5] // [5,4,3,2,1]

// Generate every odd number from 1 to n (n is function parameter)
od :: Int -> [Int]
od n
|n<=0 = []
=[x\\x<-[1..n] | isOdd x]

//Start = od 1  // [1]
//Start = od 10 // [1,3,5,7,9]
//Start = od (~4) // []

// Replicate the elements of a list a given number of times. 
// If list is [1,2,3] and given number 2 -> [1,1,2,2,3,3]
re :: [Int] Int -> [Int]
re [] _ = []
re [x:xs] n
|n<=0 = []
=(repeatn n x) ++ re xs n

//Start = re [1,1,3,4] 3 // [1,1,1,1,1,1,3,3,3,4,4,4]
//Start = re [1,2,3] 2 // [1,1,2,2,3,3]
//Start = re [] 100 // []
//Start = re [1..] (~45) // []

// Given an integer, write a function
// that returns the sum of all
// odd numbers less than it.
sumodd :: Int -> Int
sumodd x
|x==0=0
|isEven x = (x-1) + sumodd (x-2)
=(x-2) + sumodd (x-1)

//Start = okk 5 //4
// Start = f1 12 //36
// Start = f1 ~6 //0

f1aux x
| x <= 0 = 0
| isEven x = f1aux (x-1)
= x + f1aux (x-2)

okk:: Int -> Int
okk x = f1aux (x-1)


// How many different ways are there
// to distribute 'n' homework problems
// amongst 'm' number of friends?
fact :: Int -> Int
fact 1 = 1
fact x = x * fact(x-1)

nm :: Int Int -> Int
nm n m
|n<=0 || m<=0 = 0
=(fac (abs (n))) / ((fac (abs (n-m)))*(fac (abs (m))))

//Start = nm 200 4 //64684950
//Start = nm 2 4 //6
//Start = nm -4 4 //0
//Start = nm 9999999 0 //0


// Given an integer, write a function
// that will sum up its digits.

mn :: Int -> Int
mn x
| x == 0 = 0
= ((abs x) rem 10) + mn ((abs x)/10)

//Start = mn 1234 //10
//Start = mn 506 //11
// Start = mn 6 //6
//Start = mn -91 //10

/* 1. Eliminate consecutive duplicates of list elements.
 If a list contains repeated elements they should be 
 replaced with a single copy of the element
 AND a number given by parameter. 
 The order of the elements should not be changed.
 */
remove :: [Int] Int-> [Int]
remove [] a = []
remove [x] a = [x]
remove [x,y:xs] a
|x==y = [x,a] ++ remove (dropWhile ((==)x) xs) a
=[x] ++ remove [y:xs] a

//Start = remove [1] 8 // [1]
//Start = remove [] 1// []
//Start = remove [1,2,2,3,3,5] 0// [1,2,0,3,0,5]
//Start = remove [1,1,1,4,4,5,6,7,7,7,7] 9// [1,9,4,9,5,6,7,9]
//Start = remove [1,2,3,4,5,6] 0// [1,2,3,4,5,6]
//Start = remove [1,1,1,1,1,1] 5// [1,5]
//Start = remove [2,2,2,2,2,2,2] 666// [2,666]

/* 2. Given a list of sublists of Int, 
keep only the lists where all numbers
are prime numbers.
*/
areprime::[[Int]]->[[Int]]
//areprime [x:xs] = [map (filter (map (isPrime x) [x:xs]))]
areprime list = [y \\ y<-list | foldr (&&) True (map (\x=length([k\\k<-[1..x] | x rem k ==0])==2) y)]

//Start = areprime [] //[] 
//Start = areprime [[],[4,5,6],[7,11],[7..11]] //[[],[7,11]]
//Start = areprime [[1..10],[2,3,7,5],[1,3,5,7],[21]] //[[2,3,7,5]]


// 3.  Delete the n-th element of each sublist in the list.
f33 ::[[Int]] Int -> [[Int]]
f33 [] a = []
f33 list a = map (\x = removeAt (a-1) x) list

//Start = f33 [1,2,3,4,5] 1
//Start = f33 [[1,2,3],[3],[4,5,6,7],[],[0,1,6,3,5]] 3  //[[1,2],[3],[4,5,7],[],[0,1,3,5]]
//Start = f33 [[1,2,6,8,3],[9,3],[0,5,0,6,7],[],[0,1,6,3,5,8]] 3  //[[1,2,8,3],[9,3],[0,5,6,7],[],[0,1,3,5,8]]
//Start = f33 [[0],[3],[4,5,6],[],[0,1,6,9,7,3,5]] 3  //[[0],[3],[4,5],[],[0,1,9,7,3,5]]

// Eliminate consecutive duplicates of list elements.
// If a list contains repeated elements they should be 
// replaced with a single copy of the element. 
// The order of the elements should not be changed.

f14 :: [Int] -> [Int]
f14 a
|a==[]=[]
|a==[1]=[1]
//= removeDup a
|hd a==hd(tl a) = [hd(tl a)] ++ f14 (tl(tl a))
=[hd a] ++ f14 (tl a)

//Start = f14 [1] // [1]
//Start = f14 [] // []
//Start = f14 [1,2,2,3,3,5] // [1,2,3,5]
//Start = f14 [1,1,1,4,4,5,6,7,7,7,7] // [1,1,4,5,6,7,7]
//Start = f14 [1,2,3,4,5,6] // [1,2,3,4,5,6]
//Start = f14 [1,1,1,1,1,1] // [1,1,1]
//Start = f14 [2,2,2,2,2,2,2] // [2,2,2,2]

//Determine the prime factors of a given positive integer.
//Construct a flat list containing the prime factors in ascending 

primeFactors :: Int -> [Int]
primeFactors x=[y\\y<-[1..x] | x rem y==0 && isPrime y]

//Start = primeFactors 0 // []
//Start = primeFactors -5 // []
//Start = primeFactors 1 // []
//Start = primeFactors 17 // [17]
//Start = primeFactors 614889782588491410 // [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47]


// Rotate a list N places to the left.
rotate :: [Int] Int  -> [Int]
rotate [] _ = []
rotate [a] _ = [a]
rotate [a:b] 0 = [a:b]
rotate [a:b] c = rotate (b++[a]) (c-1)

//Start = rotate [1,2,3] 2 // [3,1,2]
//Start = rotate [] 3 // []
//Start = rotate [6] 5 // [6]
// Start = rotate [1,2,3,4,5,6,7,8] 3 // [4,5,6,7,8,1,2,3]

/*
Write a function that takes a list of numbers and
adds the first element, subtracts the second element,
adds the third element, subtracts the fourth element,
in this alternating repetition.

For example: [2,3,4,5,6,7] -> 2-3+4-5+6-7 = -3
*/
func :: [Int] -> Int
func [] = 0
func list = hd list - hd (tl list) + func (tl(tl list))

//Start = func [2..7] //-3
//Start = func [45,-5,63,46,-345,4321] //-4599
//Start = func [] //0

/*
Write a function that takes a list of numbers and
breaks it into two lists with alternating members from
the original list.

For example: [3,5,6,8,7,9] -> [ [3,6,7], [5,8,9] ]
*/
splitList :: [Int] -> [[Int]]
//splitList list = [[y] \\ y <- [1,3..length list]  
splitList list = [dy list 0, ni list 0]

ni :: [Int] Int -> [Int]
ni [] _ = []
ni [x:xs] n
|isOdd n = [x]++ ni xs (n+1)
=ni xs (n+1)

dy :: [Int] Int -> [Int] 
dy [] _ = []
dy [x:xs] n
|isEven n = [x]++ dy xs (n+1)
=dy xs (n+1)

//Start = splitList [56,3,87,5,234,5,0,-4] //[[56,87,234,0],[3,5,5,-4]]
//Start = splitList [1,4..50] //[[1,7,13,19,25,31,37,43,49],[4,10,16,22,28,34,40,46]]
//Start = splitList [420] //[[420],[]]
//Start = splitList []//[[],[]]

/*
Write a function that converts binary numbers to decimal numbers.

For example: 10010 = 2^4 + 2^1 = 18
*/
binaryToDecimal :: Int -> Int
binaryToDecimal x = aux3 (reverse (toList x)) 0

toList :: Int -> [Int]
toList a
|a==0 = []
=toList (a/10) ++ [a rem 10]

aux3 :: [Int] Int -> Int
aux3 [] n = 0
aux3 list n = (hd list)*2^n + aux3 (tl list) (n+1)

//Start = binaryToDecimal 10010 //18
//Start = binaryToDecimal 1010101010101 //5461

// 2. Remove all '0' from the list
isNotZero :: Int -> Bool
isNotZero n = n <> 0

remZero :: [Int] -> [Int]
remZero list = filter isNotZero list

//Start = remZero [1,9,0,2,4,7,0,5,1,8,3,0,1,2] // [1,9,2,4,7,5,1,8,3,1,2]
//Start = remZero [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0] // []
//Start = remZero [1,0,(~1),0,0,2,0,0,0,(~2),0,0,0,0,3] // [1,-1,2,-2,3]

//1. Compute the product of odd numbers from 1 to n using recursion
prodOdd :: Int -> Int
prodOdd x = prod [y \\ y<-[1..x] | isOdd y]

//Start = prodOdd 7 // 105
//Start = prodOdd 6 // 15
//Start = prodOdd 1 // 1
//Start = prodOdd (-54) //0

//3. Compute the difference between the first and the last element of each sublist.
sumss :: [[Int]] -> [Int]
sumss [] = []
sumss [x:xs] = map (\x=abs((hd x)-(last x))) [x:xs]
//|isEmpty x =[0:sums xs]
//|length x==1 = 
//Start = sumss [[1,2], [3,4,5], [6,5,9,7], [], [8]] // [1,2,1,0,0]

// 1. For every sublist, eliminates its elements
// Until the current element is a prime number
// Requirement: 
//  - Use list comrehension to determin the prime number!
//  - Use map instead of recursion
//  - Use dropWhile
//isPrimeList :: Int -> [Int]
//isPrimeList x = [i \\ i<-[1..x] | (x rem i ==0) && (length [1..x] ==2)]

//primeList::Int->[Int]
//primeList n=[y\\y<-[1..n]|isPrimeLC y && n rem y==0]
isPrimeLC n=length [y\\y<-[1..n]| n rem y==0]==2

//evans way: prim n = [x\\x<-[2..(n-1)]|n rem x ==0] == []

njo:: [[Int]] -> [[Int]]
njo [] = []
njo [x:xs] = map (\x=dropWhile (not o isPrimeLC) x) [x:xs]

//Start = njo [[1, 2, 3, 4], [9, 7, 6, 5, 4, 3, 0], [3, 5, 7, 9], [], [128, 64, 32]] // [[2,3,4],[7,6,5,4,3,0],[3,5,7,9],[],[]]
//Start = njo [[1], [4], [2]] // [[],[],[2]]
//Start = njo [[5..10], map (\x = x + 5) [1..4], [], [4,12,8,5, 4]] // [[5,6,7,8,9,10],[7,8,9],[],[5,4]]

//2. A positive number in the form like: 10, 200, 8, 1000, 40, 1, 9, 7000, 30000000
// (which has only one non-zero digit at first place) is called a "clean number"(0 is not included)
// find all clean numbers in the list of lists and write to a list
clean :: [[Int]] -> [Int]
clean b = flatten (map (filter isClean) b)
/*
cleanNum:: Int -> Bool
cleanNum n
|n <= 0 = False
|n < 10 = True
|n rem 10 <>0 = False
= cleanNum (n/10)
*/
isClean :: Int -> Bool
isClean a
|a<=0 = False
|a<10 = True
|a==10 = True
|a>10 && a rem 10 == 0 = isClean (a/10)
=False

//Start = clean [[1,2,7,10,50,102,33],[],[0,9,90],[11,980,20]] //[1,2,7,10,50,9,90,20]
//Start = clean [[1..20],[10,20..60],[30,20.. -10]]//[1,2,3,4,5,6,7,8,9,10,20,10,20,30,40,50,60,30,20,10]

//3. find the middle element of each sublists of list.(hint:use !!)
// list of even length like [0,1,2,3] choose 2
// add them together using foldr
// suggest using only one function
middle :: [[Int]] -> Int
middle [] = 0
middle [[x]] = x
middle list = foldr (+) 0 (map (\x = x !!((length x)/2)) list)

//Start =middle [[1],[1,2],[1,2,3]] //5
//Start =middle [[1],[1,2],[1,2,3],[3,3,0,8,9]] //5
//Start = middle [[10,20,30],[1,3]] //23


// Generate list of Fibonacci numbers which are less than given n and are even.
/*
fib n 
|n== 1=1
|n==2=1
=fib(n-1) + fib(n-2)
*/
f111 n = [fib x \\ x<-[1..n]| isEven (fib x) && (fib x)<n]
//Start = f111 10 
//Start= f111 100
//Start = f111 1000 // [2,8,34,144,610]
//Start = f111 100000 // [2,8,34,144,610,2584,10946,46368]
// Start = f111 1000000000 // [2,8,34,144,610,2584,10946,46368,196418,832040,3524578,14930352,63245986,267914296]
// Start = f111 10000000000 // [2,8,34,144,610,2584,10946,46368,196418,832040,3524578,14930352,63245986,267914296,1134903170,4807526976]

// Define function myLength, which returns length of a list
// You must use foldr
myLength :: [Int] -> Int
myLength a = foldr f 0 a
  where f x y=y+1

//Start = myLength [] // 0
//Start = myLength [1,2,3] // 3
//Start = myLength (take 100 [1..]) // 100
//Start = myLength [1..] // Heap full

// Define function "reverse" using foldr
myReverse :: [Int] -> [Int]
myReverse y = foldr(\ z p =p ++ [z]) [] y

//Start = myReverse [1,2,3,4,5,6,7,8] // [8,7,6,5,4,3,2,1]
//Start = myReverse [] // []
//Start = myReverse [1] // [1]

// 1. Takes out every 3rd element from the list 
third :: [Int] -> [Int]
third list = faux 1 list

faux :: Int [Int] -> [Int]
faux _ [] = []
faux n [k : l]
|n rem 3==0 = faux (n+1) l
=[k : faux (n+1) l]

//Start = third [1, 2, 3, 4, 5, 6, 7, 8, 9] // [1, 2, 4, 5, 7, 8]

// 2. Concatenates all 2nd sublists into a list 
faux2 :: Int [[Int]] -> [Int]
faux2 _ [] = []
faux2 n [k : l]
| isEven n = flatten[ k, faux2(n+1)l]
=faux2 (n+1) l

f222 :: [[Int]] -> [Int]
f222 l2 = faux2 1 l2

//Start = f222[[2, 1, 3], [], [1, 5, 2], [1, 2, 3], [0, 0], [1]] // [1, 2, 3, 1]

// 3. For every sublist, eliminates its elements 
// Until the current element is a power of 2 
power :: [[Int]] -> [[Int]]
power [] = []
power [x:xs] = map (dropWhile (not o isPower)) [x:xs]

isPower::Int-> Bool
isPower 1 = True
isPower n
|n<1=False
|n rem 2==0 =isPower (n/2)
=False
//Start = isPower 32
//Start = power [[1, 2, 3, 4], [9, 7, 6, 5, 4, 3, 0], [3, 5, 7, 9], [], [128, 64, 32]]
// [[1,2,3,4], [4,3,0], [], [], [128, 64, 32]]

// Given an integer, write a function that returns
// the largest digit in the integer.
large :: Int -> Int
large a
| a < 0 = large (abs a)
| a < 10 = a
= f3aux (a/10) (a rem 10)
//13121
//f3aux 1312 1

f3aux :: Int Int -> Int
f3aux a maxDigit //maxDigit keeps track of the max digit
| a <= 0 = maxDigit
| (a rem 10) > maxDigit = f3aux (a/10) (a rem 10)
| (a rem 10) <= maxDigit = f3aux (a/10) maxDigit


/*
List comprehensions are all about building lists.
They like to use lists.
*/
/*
When to use List Comprehension:
- You need to process a bunch of elements.
- You need to build a new list.
- You need to do operations on one or more lists.
*/

//Start = [1..10]
//Start = [x\\x<-[1..10]]
/*
[1,2,3,4,5,6,7,8,9,10] type is [Int]
*/
//Start = [[x]\\x<-[1..10]]
/*
[[1],[2],[3],[4],[5],[6],[7],[8],[9],[10]] type is [[Int]]
*/
//Start = [var\\var<-['a','b','c','d']] //['a','b','c','d']

//Start = [abc*2\\abc<-[1..10]] //[2,4,6,8,10,12,14,16,18,20]
//Start = map (\x = x*2) [1..10] //[2,4,6,8,10,12,14,16,18,20]
//Start = [x\\x<-[1..10]|isEven x] //[2,4,6,8,10]
//Start = filter isEven [1..10] //[2,4,6,8,10]

//Start = [x/2\\x<-[1..10]|isEven x] //[1,2,3,4,5]
//Start = map (\x = x/2) (filter isEven [1..10]) //[1,2,3,4,5]

//Start = [(x,y)\\x<-[1..10] & y<-[2,4..]] //[(1,2),(2,4),(3,6),(4,8),(5,10),(6,12),(7,14),(8,16),(9,18),(10,20)]
//Start = [x+y\\x<-[1..10] & y<-[2,4..]] //[3,6,9,12,15,18,21,24,27,30]
/*
Check if a sequence of Int in a list is Odd,Even,Odd,Even...
*/
//checkSeq seq = and[isEven (x+y)\\x<-seq & y<-[1..]]

//Start = checkSeq [1..10] //True
//Start = checkSeq [1,2,4,5,7,8,9] //False

//Start = [(x,y)\\x<-[1..5],y<-[1..3]] //[(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(3,2),(3,3),(4,1),(4,2),(4,3),(5,1),(5,2),(5,3)]
//Start = [(x,y,z)\\x<-[1..3],y<-[4,5],z<-[6,7,8]] //[(1,4,6),(1,4,7),(1,4,8),(1,5,6),(1,5,7),(1,5,8),(2,4,6),(2,4,7),(2,4,8),(2,5,6),(2,5,7),(2,5,8),(3,4,6),(3,4,7),(3,4,8),(3,5,6),(3,5,7),(3,5,8)]

//Coprime numbers have gcd of 1
//Start = [(x,y)\\x<-[1..10],y<-[x..10]| gcd x y == 1] //[(1,1),(1,2),(1,3),(1,4),(1,5),(1,6),(1,7),(1,8),(1,9),(1,10),(2,3),(2,5),(2,7),(2,9),(3,4),(3,5),(3,7),(3,8),(3,10),(4,5),(4,7),(4,9),(5,6),(5,7),(5,8),(5,9),(6,7),(7,8),(7,9),(7,10),(8,9),(9,10)]

/*
coprimePairs :: Int -> [(Int,Int)]
coprimePairs n = [(x,y)\\x<-[1..n],y<-[x..n]| gcd x y == 1]
*/
/*
for(int x=1; x<=n; ++x)
{
    for(int y=x; y<=n; ++y)
    {
        if(gcd(x,y)==1)
        {
            //add (x,y) to a list or array
        }
    }
}
*/

//Start = [(a,b)\\ (a,b)<-[(x,y)\\x<-[1..10],y<-[1..10]| gcd x y == 1] | a<b ] //[(1,2),(1,3),(1,4),(1,5),(1,6),(1,7),(1,8),(1,9),(1,10),(2,3),(2,5),(2,7),(2,9),(3,4),(3,5),(3,7),(3,8),(3,10),(4,5),(4,7),(4,9),(5,6),(5,7),(5,8),(5,9),(6,7),(7,8),(7,9),(7,10),(8,9),(9,10)]


reduce :: (Int, Int, Int, Int) -> (Int, Int, Int, Int)
reduce (a,b,c,d) = (z!!0,z!!1,z!!2,z!!3)
    where
    x = [a,b,c,d]
    y = gcd a (gcd b (gcd c d))
    z = [n/y\\n<-x]


//Start = reduce (2,4,6,8) //(1,2,3,4)

//Starting with RECURSION
/*
Recursion is repeating something.
Recursion has two parts.
    1. What is the repeated action?
        a. What are we doing over and over again?
        b. How does it repeat?
    2. What is the stop condition?
        a. When do we stop the recursion?
        b. How do we stop the recursion?
        c. What do we do after we stop the recursion?
*/

//Side recursion
//factorial :: Int -> Int
//factorial 4 = 4*3*2*1
/*
    1. What is the repeated action?
        a. What are we doing over and over again? -> multiplication
        b. How does it repeat? -> multiply by the next number down.
    2. What is the stop condition?
        a. When do we stop the recursion? -> when we hit 1
        b. How do we stop the recursion? -> multiply by 1
        c. What do we do after we stop the recursion? -> nothing else
*/

//code a basic step first
//Start = 4*3*2*1
factorial :: Int -> Int
factorial 0 = 1
factorial 1 = 1
factorial n
| n<0 = 0
= n*factorial (n-1) //builds off to the side. ->
//factorial 4 = 4 * factorial 3 = 4 * 3 * factorial 2 = 4*3*2*factorial 1 = 4*3*2*1
//factorial 0 = 1
//factorial -3435435 = 0


//Nested recursion
//we want to square a number repeatedly
//(((5)^2)^2)^2

//squaringThis :: Int Int -> Int
/*
    1. What is the repeated action?
        a. What are we doing over and over again? -> squaring
        b. How does it repeat? -> squaring inside of parenthesis
    2. What is the stop condition?
        a. When do we stop the recursion? -> when we've squared enough
        b. How do we stop the recursion? -> return that number itself
        c. What do we do after we stop the recursion?
*/
//Start = (5)^2
squaringThis :: Int Int -> Int
squaringThis num 0 = num
squaringThis num howMany = (squaringThis num (howMany-1))^2

//Start = squaringThis 2 3 //((((2)^2)^2)^2)

//Recursion with lists :D
// rewrite the function repeatn
// repeatn :: Int a -> [a]
//Start = repeatn 10 "Hello!"
//["Hello!","Hello!","Hello!","Hello!","Hello!","Hello!","Hello!","Hello!","Hello!","Hello!"]

//Side recursion
//Repeated action: putting elements into a list.
//Stop condition: when we've put enough elements into the list.
//Start = [1,2,3,4,5]++[6] //[1,2,3,4,5,6]
repeatnSide :: Int Int -> [Int]
repeatnSide 0 _ = []
repeatnSide howMany elem
| howMany < 0 = []
= [elem] ++ repeatnSide (howMany-1) elem
//Start = repeatnSide 10 5
//[5,5,5,5,5,5,5,5,5,5]

//Nested recursion
//Repeated action: putting elements into a list.
//Stop condition: when we've put enough elements into the list.
//Start = [5:[5]] //[5,5]
//Start = [5:[4:[3:[2:[1]]]]] //[5,4,3,2,1]
repeatnNest :: Int Int -> [Int]
repeatnNest 0 _ = []
repeatnNest howMany elem
| howMany < 0 = []
= [elem:repeatnNest (howMany-1) elem]
//Start = repeatnNest 10 5
//[5,5,5,5,5,5,5,5,5,5]

//Tail Recursion
/*
Tail recursion is recursion that is linear in time.
It takes advantage of tail recursion optimization which
is done by almost every modern compiler.
It requires an additional variable, called an accumulator.
*/
//We MUST complete evaluation at each step!!!
repeatnTail :: Int Int -> [Int]
repeatnTail howMany elem
| elem < 0 = []
= repeatnTailAux howMany elem []

repeatnTailAux :: Int Int [Int] -> [Int]
repeatnTailAux 0 _ accum = accum
repeatnTailAux howMany elem accum = repeatnTailAux (howMany-1) elem (accum ++ [elem])
//Start = repeatnTail 10 5
//[5,5,5,5,5,5,5,5,5,5]
/*
f 3 5 = fa 3 5 []
fa 3 5 []
fa 2 5 [5]
fa 1 5 [5,5]
fa 0 5 [5,5,5]
*/
//addUp 2 5 = 2+ 3+4+5
addUp :: Int Int -> Int
addUp a b
| a > b = addUp b a
= addUpAux a b []

addUpAux :: Int Int [Int] -> Int
addUpAux a b accum
| a > b = sum accum
= addUpAux (a+1) b (accum++[a])
/*
au 2 5
aua 2 5 []
aua 3 5 [2]
aua 4 5 [2,3]
aua 5 5 [2,3,4]
aua 6 5 [2,3,4,5]
*/

/*
Given a list sublists of numbers, keep only
the sublists that have all prime numbers.
*/
//filter :: (a->Bool) [a] -> [a]
//Start = filter isEven [1,2,3,4,5] //[2,4]
//Start = filter condEven [[2,4..20],[23,25],[1..5],[2,4,6]]
condEven :: [Int] -> Bool
condEven ourList = and(map isEven ourList)
//Start = condEven [1,2,4,6,8]
// map func [a,b,c] -> [func a, func b, func c]

//Start = primeListFilter [[1,2,3],[4,9,25],[1,3,7,12,14],[]]
primeListFilter :: [[Int]] -> [[Int]]
primeListFilter ourList = filter condPrime (filter notEmpty ourList)

notEmpty :: [Int] -> Bool
notEmpty someList = not(isEmpty someList)

condPrime :: [Int] -> Bool
condPrime ourList = and(map isPrimee ourList)

isPrimee :: Int -> Bool
isPrimee n = not(or(map (dividable n) [2..(n-1)]))

dividable :: Int Int -> Bool
dividable n check = n rem check == 0

//Start = isPrime 10


/*
map something (map someotherthing somelist)
map (map something) somelist
*/
add3 n = n+3
times2 n = n*2

//Start = map add3 (map times2 [1..5]) //[5,7,9,11,13]
// map add3 [2,4,6,8,10]
// [5,7,9,11,13]
//Start = map (map add3) [[1..5],[5,8,3],[],[0]] //[[4,5,6,7,8],[8,11,6],[],[3]]
// [ map add3 [1..5], map add3 [5,8,3], map add3 [], map add3 [0] ]
// [ [4,5,6,7,8], [8,11,6], [], [3] ]

/*
isPrime x = x == last(sieve2 x)

sieve2 :: Int -> [Int]
sieve2 x = [1] ++ sieveY [2..x] 1

sieveY :: [Int] Int -> [Int]
sieveY numList index
| isEmpty numList = numList
| index == len = [a] ++ sieveY (tl numList) 1
| b rem a  == 0 = sieveY (removeAt index numList) index
= sieveY numList (index+1)
    where
    len = length numList
    a = hd numList
    b = numList!!index

sieve :: Int -> [Int]
sieve n = sieveX (repeatn n True) n 2 4

sieveX :: [Bool] Int Int Int -> [Int]
sieveX boolList n i j
| j > n = sieveX boolList n (i+1) ((i+1)^2)
| i > toInt(sqrt(toReal n)) = [i\\i<-[1..n]|boolList!!(i-1)]
| boolList!!(i-1) = sieveX (updateAt (j-1) False boolList) n i (j+i)
= sieveX boolList n (i+1) ((i+1)^2)

Start = sieve2 10000
//Start = sieve 10000


/**2
  * Write a function that takes a list of integers and returns a list of
  * result integers based on how many integers were in the parameter list.
  * For 1 integer 'a', it will return that integer modulus 2. (a rem 2)
  * For 2 integers 'a','b' , it will return a list of all integers from the first to the second. [a..b]
  * For 3 integers 'a','b','c' , it will return (a*(b^c))
  * For 4 integers 'a','b','c','d', it will return a list of the sum of 'a' and 'b' and the sum of 'c' and 'd'.
  */

Listing :: [Int] -> [Int]
Listing myList
| length myList == 0 = []
| length myList == 1 = [(hd myList) rem 2]
| length myList == 2 = [(hd myList)..(last myList)]
| length myList == 3 = [(hd myList)*(myList!!1 ^ last myList)]
| length myList == 4 = [myList!!0 + myList!!1 , myList!!2 + myList!!3]

ListingAlt :: [Int] -> [Int]
ListingAlt [] = []
ListingAlt [a] = [a rem 2]
ListingAlt [a,b] = [a..b]
ListingAlt [a,b,c] = [a*(b^c)]
ListingAlt [a,b,c,d] = [a+b,c+d]

//[ elem, elem]
//(1,2,3)

//Start = someFunc isEven ((<)3) [1..10]

// a < b //infix form <
//(<) a b //prefix form
//Start = Listing [5] //[1]

//Start = Listing [4,10] //[4,5,6,7,8,9,10]

//Start = Listing [3,5,2] //[75]

//Start = Listing [13,29,1030,307] //[42,1337]

//Start = Listing [] //[]

//currying
//when your function doesn't have enough variables.

//Start =  ((+)3) 1

grid :: Int -> [[(Int,Int)]]
grid n = [[(rowNo,colNo)\\colNo<-[1..n]]\\rowNo<-[1..n]]

//Start = grid 5

build :: Int -> [[Int]]
build n = [ (repeatn (r-1) 0)++[0..(n-r)] \\ r<-[1..n] ]
//Start = build 10
*/

/**1
  * Write a function, that takes a list of functions, and a list of
  * tuples (Int, Int) where the first Int indicates which function to
  * use and the second Int acts as a parameter and returns a list of
  * the results.
  
  * For example: Router [isEven,isOdd] [(1,2),(2,4),(1,57)] = [True, False, False]
  */

Router :: [(a->b)] [(Int,a)] -> [b]
Router listFunc listRoute
| isEmpty listFunc || isEmpty listRoute = []
= [ (listFunc!!(funcNum-1)) param\\ (funcNum, param) <-listRoute]

//Start = Router [isEven,isOdd] [(1,2),(2,4),(1,57)] //[True, False, False]
//Start = Router [((+)1),((*)2),((^)2),((rem) 100)] [(4,13),(2,23),(3,5),(1,1336),(4,23)] //[9,46,32,1337,8]
//Start = Router [(\x = [1..x]),(\x = [n\\n<-[1..x]|x rem n ==0]),(\x = [x,x*2..x*10])] [(2,36),(1,13),(3,5),(2,128),(3,1)]  
//[[1,2,3,4,6,9,12,18,36],[1,2,3,4,5,6,7,8,9,10,11,12,13],[5,10,15,20,25,30,35,40,45,50],[1,2,4,8,16,32,64,128],[1,2,3,4,5,6,7,8,9,10]]
//Start = Router [] [(4,13),(2,23),(3,5),(1,1336),(4,23)] //[]
//Start = Router [isEven,isOdd] [] //[]

/**2
  * Write a function that takes a list of integers and returns a list of
  * result integers based on how many integers were in the parameter list.
  * For 1 integer 'a', it will return that integer modulus 2. (a rem 2)
  * For 2 integers 'a','b' , it will return a list of all integers from the first to the second. [a..b]
  * For 3 integers 'a','b','c' , it will return (a*(b^c))
  * For 4 integers 'a','b','c','d', it will return a list of the sum of 'a' and 'b' and the sum of 'c' and 'd'.
  */
Listing :: [Int] -> [Int]
Listing [] = []
Listing [a]=[a rem 2]
Listing [a,b] = [a..b]
Listing [a,b,c] = [a*(b^c)]
Listing [a,b,c,d] = [(a+b),(c+d)]

//Start = Listing [5] //[1]
//Start = Listing [4,10] //[4,5,6,7,8,9,10]
//Start = Listing [3,5,2] //[75]
//Start = Listing [13,29,1030,307] //[42,1337]
//Start = Listing [] //[]

/**3
  * Write a function that checks if a list of numbers is odd,even,odd,even...
  * For exmaple: SeqCheck [1,2,3,4,6] = False because 4 is even, but 6 is not odd.
  */

SeqCheck :: [Int] -> Bool
SeqCheck [] = False
//SeqCheck [x,y:xs]
//|isEven x && isOdd y && SeqCheck xs = True
//=False  
SeqCheck seq = and[isEven (x+y)\\x<-seq & y<-[1..]]

//Start = SeqCheck [1..10] //True
//Start = SeqCheck [1,2,3] //True
//Start = SeqCheck [2,3,4] //False
//Start = SeqCheck [1,3,4,5] //False
//Start = SeqCheck [1,2,3,4,6,7] //False
//Start = SeqCheck [] //False

/**4
  * Write a function that checks if each elements in the list appear even times.
  
  * For example, checkEven [1,1,2,2,2,2,3,5,3,5] = True
  */
checkEven :: [Int] -> Bool
checkEven [] = False
checkEven nums = and[isEven(length(filter ((==)x) nums)) \\x<-nums]

//Start = checkEven [1,1,2,2,2,2,3,5,3,5] // True
//Start = checkEven [1,1,2,2,1] // False
//Start = checkEven [] //False  

/**5
  * Write a function that takes two vectors, represented as lists, and returns their dot product.
  
  * The dot product of two vectors can be computed as:
  
  * < xa, xb, xc, ...> * < ya, yb, yc, ...> = (xa*ya) + (xb*yb) + (xc*yc) + ...
  
  * For example: DotProd [4,6,3] [6,3,7] = 24+18+21 = 63
  */

DotProd :: [Int] [Int] -> Int
DotProd vec1 vec2 = sum [x*y\\x<-vec1 & y<-vec2]

//Start = DotProd [4,6,3] [6,3,7] //63
//Start = DotProd [6,3,7] [4,6,3] //63
//Start = DotProd [5,2,6,8,3] [5,-8,5,-3,-5] //0

/**6
Given a list of characters, split it into a tuple in which the first part only contains digits ('0'..'9'),
the second part contains the rest. */
TwoLists :: [Char] -> ([Char],[Char])
TwoLists list = ([x\\x<-list | isMember x ['0'..'9']], [y\\y<-list | isMember y ['a'..'z']])

//Start = TwoLists  ['1', 'a', '2', 'b', '3'] // (['1','2','3'],['a','b'])
//Start = TwoLists [] // ([],[])

/**7
 Given a list of lists, for each list, extract the first, middle and last element. */
Points3 :: [[Int]] -> [(Int, Int, Int)]
//Points3 list = [(hd list, list!!(length (list/2)), last list)]
Points3 bigList = [(hd subList, subList!!((length subList)/2), last subList)\\subList<-bigList]

//Start = Points3 [[1..9], [2..6], [3..11], [1..10]] // [(1,5,9),(2,4,6),(3,7,11),(1,6,10)]
//Start = Points3 [[]] //[]

/**8
Find the 'unique' right triangle in the list eg. (3,4,5) and (4,3,5) are the same triangle. 
only one will appear in the answer list [(3,4,5),(4,3,5)] -> [(3,4,5)] */

f81::[(Int,Int,Int)]->[(Int,Int,Int)]
f81 list = f8aux (filter checkValid list)

f8aux :: [(Int,Int,Int)]->[(Int,Int,Int)]
f8aux [] = []
f8aux [a:b] = [a] ++ f81 (filter (notDup a) b)

checkValid :: (Int,Int,Int) -> Bool
checkValid (x,y,z) = (a<>b)&&(b<>c)&&(a+b>c)&&(a^2 + b^2 == c^2)
    where
    sorted = sort[x,y,z]
    a = sorted!!0
    b = sorted!!1
    c = sorted!!2

notDup :: (Int,Int,Int) (Int,Int,Int) -> Bool
notDup (x,y,z) (a,b,c) = (sort list1) <> (sort list2)
    where
    gcd1 = gcd x (gcd y z)
    gcd2 = gcd a (gcd b c)
    list1 = map (\elem = elem/gcd1) [x,y,z]
    list2 = map (\elem = elem/gcd2) [a,b,c]

//Start = f81 [(3,4,5),(4,5,6),(4,5,3),(6,8,10),(10,5,8),(-3,4,5)] //[(3,4,5)]
//Start = f81 [(1,1,1),(5,4,3),(3,4,5),(0,0,0)] //[(5,4,3)]
isP :: Int -> Bool
isP n = and[n rem x <> 0\\ x<-[2..(n-1)] ]

//Start = isP 7

/**9
Use foldr to check if the square root of each integer in a list are all integers. */

f91 :: [Int] ->Bool
f91 myList = foldr (\ x y = y && ((sqrt (toReal x))==(toReal(toInt (sqrt (toReal x)))))) True myList

//Start = f91 [] //True
//Start = f91 [4,16,9] //True
//Start = f91 [1,8] //False

/**10 Insert sum of elements as last element in every sublist of a list. */

addSum :: [[Int]] -> [[Int]]
addSum bigList = [subList++[sum subList]\\subList<-bigList]

//Start = addSum [[1,2], [3,4,5], [6,5,9,7], [], [8]] //[[1,2,3],[3,4,5,12],[6,5,9,7,27],[0],[8,8]]



// 1. Write a function that will return the second to last digit in a number. Return 0 if there is no second digit.
scnd :: Int -> Int
scnd x = abs((x/10) rem 10)

//Start = scnd 1234 //3
//Start = scnd 5 //0
//Start = scnd -5564 //6


// 2. Write a function that will subtract numbers in a list from the first one. Your solution must use 'foldr' or 'foldl'.
// Return 0 for an empty list.
sub :: [Int] -> Int
sub [] = 0
sub [a:b] = foldl (-) a b

//Start = sub [10,1,2,3] //4
//Start = sub [1,2,3,4] //-8
//Start = sub [1000,500,250,125] //125
//Start = sub [] //0


// 3. Write a function that returns all prime divisors of a number. e.g. f3 36 = [1,2,3]

//f3 :: Int -> [Int]
//f3 0 = []
//f3 x = removeDup(f3aux x 2 [1])

f3auxx :: Int Int [Int] -> [Int]
f3auxx x y list
| x == 1 = list
| x rem y == 0 = f3auxx (x/y) 2 (list++[y])
= f3auxx x (y+1) list

//Start = f3auxx 5 3 [1,2,3]
//Start = f3 36 //[1,2,3]

//Start = f3 524287  //[1,524287]

//Start = f3 0 //[]

//delel :: 

/*
// 4. Write a function that reverses tuples from a list if the tuple members sum up to an even number.

f4 :: [(Int, Int)] -> [(Int, Int)]
f4 myList = [f4aux x\\x<-myList]

f4aux :: (Int,Int) -> (Int,Int)
f4aux (a,b)
| isEven (a + b) = (b,a)
= (a,b)
//Start = f4 [(1,2),(3,4),(5,7)] //[(1,2),(3,4),(7,5)]

//Start = f4 [(-1,3),(12,1),(0,0),(-4,-2)] //[(3,-1),(12,1),(0,0),(-2,-4)]

//Start = f4 [] //[]


// 5. Write a function that takes every number in a list and generates a sublist of its first 5 multiples. Your solution must use 'map'.

f5 :: [Int] -> [[Int]]
f5 myList = [[x,2*x..5*x]\\x<-myList]

//Start = f5 [1..3] //[[1,2,3,4,5],[2,4,6,8,10],[3,6,9,12,15]]

//Start = f5 [4,~3,5,~6] //[[4,8,12,16,20],[-3,-6,-9,-12,-15],[5,10,15,20,25],[-6,-12,-18,-24,-30]]

//Start = f5 [] //[]


// 6. Given an integer n, find the minimal k such that

// k = m! (where m! = 1 * 2 * ... * m) for some integer m; k >= n.

// In other words, find the smallest factorial which is not less than n.

// example: leastfactorial 17 = 24.  because 17 < 24 = 4! = 1 * 2 * 3 * 4, while 3! = 1 * 2 * 3 = 6 < 17

leastfactorial :: Int -> Int
leastfactorial n = hd(dropWhile ((>)n) [prod[1..m]\\m<-[1..]])

//Start = leastfactorial 17 // 24

//Start = leastfactorial 1 // 1

//Start = leastfactorial 5 // 6

//Start = leastfactorial 25 // 120


// 7. Write a function that checks if a list of numbers is odd,even,odd,even...

// e.g. for [1,2,3,4,6] it is false because 4 is even, but 6 is not odd.

f7 :: [Int] -> Bool
f7 [] = False
f7 seq = and[isEven (x+y)\\x<-seq & y<-[1..]]

//Start = f7 [1..10] //True

//Start = f7 [1,2,3] //True

//Start = f7 [2,3,4] //False

//Start = f7 [1,3,4,5] //False

//Start = f7 [1,2,3,4,6,7] //False

//Start = f7 [] //False


// 8. Write a function that removes consecutive duplicates in a list.

f8 :: [Int] -> [Int]
f8 [] = []
f8 [a] = [a]
f8 [a,b:c]
| a == b = f8 (dropWhile ((==)a) c)
= [a:f8 [b:c]]

//Start = f8 [4,5,6,6,8,2,2,2,4,0,0,0,7,0,5,0,0,4] //[4,5,8,4,7,0,5,4]

//Start = f8 [1,0,0,2,0,3,3,0,6,7,0,7,7] //[1,2,0,0,6,7,0]

//Start = f8 [2,0,0,6,7,5,0,8,0,5,0,0,0] //[2,6,7,5,0,8,0,5] 


// 9. Write a function that takes a tuple of three lists and generates a list of triple tuples.

// The triple tuple is only generated if the i-th member of the first list multiplied by the

// i-th member of the second list equals the i-th member of the third list.

// e.g. for ([1,2,3,4,5],[2,4,6,8,10],[2,8,17,32,45]) the result is [(1,2,2),(2,4,8),(4,8,32)]

f9 ::([Int],[Int],[Int])->[(Int,Int,Int)]
f9 (listA, listB, listC) = [(a,b,c)\\a<-listA & b<-listB & c<-listC | a*b == c]

//Start = f9 ([2,2,2,2,2,2],[1,2,3,4,5,6,7,8],[2,4,6,6,10])//[(2,1,2),(2,2,4),(2,3,6),(2,5,10)]

//Start = f9 ([1,2,3,4,5],[2,4,6,8,10],[2,8,1,32,45])//[(1,2,2),(2,4,8),(4,8,32)]

//Start = f9 ([1,0,1,0,1,0],[3,4,5,6,8],[3,0,5,0,0])//[(1,3,3),(0,4,0),(1,5,5),(0,6,0)]


// 10. Write a function that checks if a number is a Mersenne Prime.

// A Mersenne Prime is a prime number that is 1 less than a power of 2. example: 7 = (2^3) - 1 = 8-1


f10 :: Int -> Bool
f10 x
| x<=0 = False
= isPrime x && isLog2 (x+1)

isLog2 :: Int -> Bool
isLog2 x = result == toReal(toInt result)
    where
    y = toReal x
    result = (log10 y)/(log10 2.0)

isPrime :: Int -> Bool
isPrime x = length(primeFactorization x 2 [1]) == 2

primeFactorization :: Int Int [Int] -> [Int]
primeFactorization x y list
| x == 1 = list
| x rem y == 0 = primeFactorization (x/y) 2 (list++[y])
= primeFactorization x (y+1) list

//Start = f10 7 //True

//Start = f10 1 //False

//Start = f10 (~235) //False

//Start = f10 2147483647 //True

//Start = f10 0 //False
*/

//create a list of the first n tribonacci numbers
//tribonacci sequence is an=an-1+an-2+an-3

tribonacci::Int -> Int
tribonacci 0 = 1
tribonacci 1 = 1
tribonacci 2 = 2
tribonacci n=(n-1)+(n-2)+(n-3)
//Start = tribonacci 5

triList :: Int -> [Int]
triList n
|n<=0 = []
=triList (n-1) ++ [tribonacci n]
//Start = triList 5

trii :: Int -> [Int]
trii n = [tribonacci x \\ x<-[1..n]]
//Start = trii 5


