// Примеры к курсу "Увлекательное введение в F#"
// Дмитрий Сошников 
// dmitri@soshnikov.com, twitter.com/shwars, http://blog.soshnikov.com
// -------------------------------------------------------------------
// ----------- Лекция 1а ---------------------------------------------

let rec rpt n f = 
     if n=0 then fun x->x
     else f >> rpt (n-1) f

#r @"FSharp.PowerPack.dll"

open Microsoft.FSharp.Math
open System

let mandelf c (z:Complex) = z*z+c
let ismandel c = Complex.Abs(rpt 20 (mandelf c) Complex.zero)<1.0

// Масштабирование
let scale (x:float,y:float) (u,v) n = float(n-u)/float(v-u)*(y-x)+x;;

(* Строим на консоли *)
for i=1 to 40 do
 for j=1 to 40 do
   let lscale = scale (-1.2,1.2) (1,40) in
   let t = complex (lscale j) (lscale i) in 
   Console.Write(if ismandel t then "*" else " ")
 Console.WriteLine("")


(* Строим в окне Windows Forms *)

open System.Drawing
open System.Windows.Forms

let form =
   let image = new Bitmap(400, 400)
   let lscale = scale (-1.2,1.2) (0,image.Height-1)
   for i = 0 to (image.Height-1) do
     for j = 0 to (image.Width-1) do
       let t = complex (lscale i) (lscale j) in
       image.SetPixel(i,j,if ismandel t then Color.Black else Color.White)
   let temp = new Form()
   temp.Paint.Add(fun e -> e.Graphics.DrawImage(image, 0, 0))
   temp.Show()
   temp

[<STAThread>]
do Application.Run(form)
