// Примеры к курсу "Увлекательное введение в F#"
// Дмитрий Сошников 
// dmitri@soshnikov.com, twitter.com/shwars, http://blog.soshnikov.com
// -------------------------------------------------------------------
// ----------- Лекция 2 ----------------------------------------------

// Деревья
type 't tree = Nil | Node of 't*'t tree*'t tree

let rec count = function
       | Nil -> 0
       | Node(x,l,r) ->
           1+count l+count r

// Списки
type 't list = [] | (::) of 't*'t list

let rec length = function
    | [] -> 0
    | h::t -> 1+length t

length [1;2;3]

List.fold max 1 [1;6;8;5;3;6]

// Последовательности
{1..10} |> Seq.map ((*)2)

open System

let Rnd = new Random()
let rec rand n = 
  seq {
    yield Rnd.Next(0,n)
    yield! (rand n)
  }

#load @"FSharp.Charting.fsx"
open FSharp.Charting
rand 100 |> Seq.take 100 |> Chart.Line

let rec nat = seq {
     yield 1
     yield! (Seq.map ((+)1) nat)
  }

let nat = seq {
     let x = ref 1
     while true do
       yield !x
       x := !x+1
  }


let fact n = 
  nat |> Seq.scan (*) 1 
      |> Seq.nth n

fact 5


open System.IO
open System

let ReadLines fn =
  seq { use inp = File.OpenText fn in
        while not(inp.EndOfStream) do
            yield (inp.ReadLine())
      }

ReadLines @"book.txt"
 |> Seq.map (fun s->s.Split([|';';',';'!';'?';'-';'.';'\'';' '|]))
 |> Seq.concat
 |> Seq.filter (fun s->s.Length>3)
 |> Seq.maxBy (fun s->s.Length)


let ReadSet fn = 
  ReadLines fn
  |> Seq.map (fun s->s.Trim())
  |> Seq.fold (fun acc x -> Set.add x acc) Set.empty

let pos = ReadSet @"positive.txt"
let neg = ReadSet @"negative.txt"
let wt s = 
  match s with
    | _ when Set.contains s pos -> 1
    | _ when Set.contains s neg -> -1
    | _ -> 0

let weight (s:string) = 
  s.Split([|' ';'!';'.';',';';';'?'|])
  |> Array.fold (fun acc s -> acc+wt s) 0

ReadLines @"book.txt"
|> Seq.map weight
|> FSharpChart.Line
