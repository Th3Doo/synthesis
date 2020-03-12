module Synthesis

open System.Net.Sockets

let abelar n = n>12 && n<3097 && n%12=0

let area b h = match b<0.0 || h<0.0 with
                | false -> (b/2.0)*h
                | true -> failwith "Failed"

let zollo n = match n<0 with
                | true -> n*(-1)
                | false -> n*2

let min a b = match a<b with
                | true -> a
                | false -> b

let max a b = match a<b with
              | true -> b
              | false -> a

let ofTime h m s = (h*60*60) + (m*60) + s

let toTime n = match n > 0 with
                | true -> let s = n%60
                          let temp = n/60
                          let m = temp%60
                          let h = temp/60
                          (h,m,s)
                | false -> (0,0,0)

let digits n =
    let rec digitFunc num acc=
        match num <10 && (num*(-1) < 10) with
        | true -> acc+1
        | false -> digitFunc (num/10) (acc+1)
    digitFunc n 0

let minmax (num1, num2, num3, num4) = 
    num1 |> min num2 |> min num3 |> min num4,num1 |> max num2 |> max num3 |> max num4

let isLeap _ =
    failwith "Not implemented"

let month _ =
    failwith "Not implemented"

let toBinary _ =
    failwith "Not implemented"

let bizFuzz _ =
    failwith "Not implemented"

let monthDay _ _ =
    failwith "Not implemented"

let coord _ =
    failwith "Not implemented"