// Примеры к курсу "Увлекательное введение в F#"
// Дмитрий Сошников 
// dmitri@soshnikov.com, twitter.com/shwars, http://blog.soshnikov.com
// -------------------------------------------------------------------
// ----------- Лекция 1 ----------------------------------------------

// Простейшие вычисления

let twice x = x*2
twice 5
twice (twice 5)
(twice>>twice)5
5 |> twice |> twice
twice <| 5 |> twice

let twice x = x*2
twice 5
twice (twice 5)
(twice >> twice) 5
let quad = twice >> twice
5 |> twice |> twice
twice <| 5 |> twice
let twice = fun x->x*2
(fun x->x*3) >> twice <| 5


let quad = twice >> twice
let quad x = x |> twice |> twice

// Лямбда-выражения

let twice = fun x->x*2
(fun x->x*3) >> twice <| 5

// Описание циклических процессов

let rec sum a b =
    if a>b then 0
    else a+sum (a+1) b

sum 1 100

// Обобщение цикличности

let rec iter a b f i =
    if a>b then i
    else f a (iter (a+1) b f i)

let sum a b = iter a b (+) 0

sum 1 100

open System.Numerics

let fact n = iter 1 n (*) 1
let factI n = iter 1 n (fun n acc -> (BigInteger n)*acc) 1I
factI 1000

fact 7 |> factI 

// Вычисляем ряд Тейлора
let power x n = iter 1 n (fun n acc -> acc*x) 1.

let myexp x = 
      iter 0 6
        (fun n acc -> acc+(power x n)/(fact n|>float)) 
        0.

myexp 1.

// Интегрирование
let integrate a b f = 
   let n = 100
   let h = (b-a)/float(n)
   iter 0 n (fun i acc -> 
              let x = a+h*float(i)
              acc+h*f(x)) 0.

integrate 0. 3.14 sin

let compute a b =
   let n=100
   let h=(b-a)/(float n)
   {0..n} 
   |> Seq.map (fun x->a+h*float(x))
   |> Seq.map (sin >> (*)h)
   |> Seq.sum

compute 0. 3.14

// Хвостовая рекурсия

let fact n = 
    let rec fact' acc n =
        if n=1 then acc
        else fact' (acc*n) (n-1) 
    fact' 1 n

fact 4


let iter a b f i = 
    let rec iter' a acc =
        if a>b then acc
        else iter' (a+1) (f a acc)
    iter' a i

13 |> fact |> factI // Не пробуйте это выполнять!

// Каррирование
let plus (x,y) = x+y
let plus = fun (x,y) -> x+y
let plus x y = x+y
let plus = fun x y -> x+y
let plus = fun x -> fun y -> x+y

let fact = 
    let rec fact' acc n =
        if n=1 then acc
        else fact' (acc*n) (n-1) 
    fact' 1 

fact 5

// Избавляемся от явных лямбда-выражений
let f x = 2*x+1
let f = (*)2 >> (+)1

f 5
