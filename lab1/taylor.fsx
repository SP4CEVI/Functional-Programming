let eps:float = 0.0000001

let f x:float =  (1. + x ** 2.) / 2. * System.Math.Atan(x) - x / 2.
let f_iter (x:float) (n:int) = (-1.) ** n * (-1.) * (x ** n * x ** n * x) / (4. * float(n) * float(n) - 1.)

let a:float = 0.1
let b:float = 0.6
let n:int = 10

let taylor_naive x:float = 
    let mutable i:int = 1
    let mutable sum:float = 0.
    while abs (f_iter x i) > eps do
        sum <- sum + f_iter x i
        i <- i + 1
    sum    
        
let taylor x:float =
    let mutable sum:float = 0.
    let mutable befor_s:float = 1.
    let mutable befor_sd:float = 5.
    let mutable befor:float = 1. / 3.
    let mutable befor_x:float = x ** 3.
    let mutable f_iter:float = befor * befor_x
    while abs (f_iter) > eps do
        sum <- sum + f_iter
        befor_s <- befor_s + 2.
        befor_sd <- befor_sd + 2.
        befor <- befor * befor_s / befor_sd
        befor_x <- befor_x * x * x
        f_iter <- befor_x * befor
    sum



let main:unit =
    for i=0 to n do
        let x:float = a + (float i) / (float n) * (b - a)
        printfn "%5.2f  %10.6f  %10.6f   %10.6f" x (f x) (taylor_naive x) (taylor x)

main