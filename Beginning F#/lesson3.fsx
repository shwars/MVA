let task1 = async { return 10+10 }
let task2 = async { return 20+20 }
Async.RunSynchronously (Async.Parallel [ task1; task2 ])


let map' func items =
     let tasks =
         seq {
             for i in items -> async { return (func i) } }
     Async.RunSynchronously (Async.Parallel tasks)

let rec fib x =
   if x<2 then 1
   else fib(x-1) + fib(x-2)

#time
List.map (fun x -> fib(x)) [30..40]
map' (fun x -> fib(x)) [30..40]

open System.Net
open System.IO
let httpAsync (url:string) =
       async {
         let req = WebRequest.Create(url)             
         let! resp = req.AsyncGetResponse()
         use stream = resp.GetResponseStream() 
         use reader = new StreamReader(stream) 
         let! text = Async.AwaitTask(reader.ReadToEndAsync())
         return text }

let books = [
 ("Pride and Predjudice", "http://www.gutenberg.org/cache/epub/1342/pg1342.txt");
 ("Tom Sawyer", "http://www.gutenberg.org/cache/epub/74/pg74.txt");
 ("Alice in Wonderland", "http://www.gutenberg.org/cache/epub/11/pg11.txt")];

books 
  |> List.map snd
  |> List.map (fun url -> 
                    async {
                       let! c = httpAsync url
                       let x = c.Split([|' ';'.';',';'!';'?';'-';':';';'|])
                               |> Seq.filter (fun x->x.Length>0)
                               |> Seq.fold (fun acc x -> acc+wt x) 0
                       return x })
  |> Async.Parallel
  |> Async.RunSynchronously


books 
 |> List.map snd
 |> List.map (fun url ->
               async {
                 let! c = httpAsync url
                 let x = c.Split([|' ';'.';',';'!';'?';'-';':';';'|])
                          |> Seq.filter (fun s->s.Length>1)
                          |> Seq.map wt
                          |> Seq.reduce (+)
                 return x })
 |> Async.Parallel
 |> Async.RunSynchronously
