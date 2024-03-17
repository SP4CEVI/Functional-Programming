let eps: float = 0.00000001

let f x: float = (1. + x ** 2.) / 2. * System.Math.Atan(x) - x / 2.

let rec f_it (x: float) (n: int) =
    if n = 0 then
        1.
    else
        let f_it_minus_1 = f_it x (n - 1)
        f_it_minus_1 * (x ** (2. * (float n) + 1.)) / (4. * float(n) * float(n) - 1.)

let a: float = 0.1
let b: float = 0.6
let n: int = 10

let rec taylor_naive x i sum =
    if i <= n then
        let term = f_it x i
        taylor_naive x (i + 1) (sum + term)
    else
        sum

let rec taylor x sum befor_s befor_sd befor befor_x =
    let f_it = befor_x * befor
    if abs f_it > eps then
        taylor x (sum + f_it) (befor_s + 2.) (befor_sd + 2.) (befor * (befor_s / befor_sd)) (befor_x * x * x)
    else
        sum

let main =
    for i = 0 to n do
        let x: float = a + float i / float n * (b - a)
        let taylor_result = taylor_naive x 0 0.
        printfn "%5.2f  %10.5f  %10.5f   %10.5f" x (f x) (f_it x 1) (taylor x 0. 1. 5. (1./3.) (x ** 3.))

main
