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

let isLeap year = match year > 1581 with
                  | true -> (year%100 = 0 && year%400 = 0 && year%4 = 0) || (not(year%100 = 0) && year%4 = 0)
                  | false -> failwith "before 1582"

let month num = match num with
                | 1 -> "January", 31
                | 2 -> "February", 28
                | 3 -> "March", 31
                | 4 -> "April", 30
                | 5 -> "May", 31
                | 6 -> "June", 30
                | 7 -> "July", 31
                | 8 -> "August", 31
                | 9 -> "September", 30
                | 10 -> "October", 31
                | 11 -> "November", 30
                | 12 -> "December", 31
                | notMonth -> failwith "not a valid month"

let toBinary num =
    match num < 0 with
    | false -> let rec toBin num stop text acc = 
                   match stop with
                   | false -> match num%2 with
                              | 0 -> toBin (num/2) (num/2 = 0) ("0" + text) (acc + 1)
                              | 1 -> toBin (num/2) (num/2 = 0) ("1" + text) (acc + 1)
                   | true -> text.Remove(acc, 4)
               toBin num false "-end" 0
    | true -> failwith "not a positive integer"    

let bizFuzz n =
    let rec div start num (acc1,acc2,acc3) =
        match start < num + 1 && num > -1 with
        | true -> match start%3 = 0 && start%5 = 0 with
                  | true -> div (start + 1) (num) (acc1 + 1, acc2 + 1, acc3 + 1)//div (start + 1) (num) (acc1 + 1, acc2, acc3)
                  | false -> match start%3 = 0 && not(start%5 = 0) with
                             | true -> div (start + 1) (num) (acc1 + 1, acc2, acc3)//div (start + 1) (num) (acc1, acc2 + 1, acc3)
                             | false -> match not(start%3 = 0) && start%5 = 0 with
                                        | true -> div (start + 1) (num) (acc1, acc2 + 1, acc3)//div (start + 1) (num) (acc1 + 1, acc2 + 1, acc3 + 1)
                                        | false -> div (start + 1) (num) (acc1, acc2, acc3)
        | false -> (acc1,acc2,acc3)
    div 1 n (0,0,0)

let monthDay d y =
    match y > 1581 && d > 0 with
    | true -> let rec getMonth day year acc=
                  let currentMonth,currentMonthDays = month acc
                  match acc = 2 with
                  | true -> match isLeap y with
                            | true -> match day < currentMonthDays+2 with 
                                      | true -> currentMonth
                                      | false -> getMonth (day-(currentMonthDays+1)) (year) (acc+1)
                            | false -> match day < currentMonthDays+1 with 
                                       | true -> currentMonth
                                       | false -> getMonth (day-currentMonthDays) (year) (acc+1)
                  | false -> match day < currentMonthDays+1 with 
                             | true -> currentMonth
                             | false -> getMonth (day - currentMonthDays) (year) (acc+1)   
              getMonth d y 1
    | false -> failwith "invalid year"

let coord a b : float =
    let (x1,y1) = a
    let x2,y2 = b
    let dist = sqrt (((x1-x2)*(x1-x2)) + ((y1-y2)*(y1-y2)))
    failwith "ni"

let sqrt num = 
    let rec calculate guess inc =
        match inc with 
        | 10 -> guess
        | _ -> let g = (guess + num/guess) / 2.0
               calculate g (inc+1)
    match num <= 0.0 with 
    | true -> failwith "impossible"
    | false -> calculate (num/2.0) 0

