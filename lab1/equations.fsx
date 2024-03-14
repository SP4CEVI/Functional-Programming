let rec dichotomy f (a: float) (b: float) =
    let eps = 0.000001
    let xn = (a + b) / 2.
    let fa = f a
    let fb = f b

    if abs (f xn) < eps then
        xn
    else if fa < fb then
        if (f xn) < 0. then dichotomy f xn b else dichotomy f a xn
    else 
        if (f xn) > 0. then dichotomy f xn b else dichotomy f a xn


let rec iterations phi x0 =
    let eps = 0.000001

    if abs (x0 - (phi x0)) < eps then
        x0
    else
        let next = phi x0
        iterations phi next


let newthon f f' x0 =
    let phi x : float = x - (f x) / (f' x)
    iterations phi x0


let f18 x : float = x + sqrt (x) + System.Math.Cbrt (x) - 2.5
let f19 x : float = x - 1. / (3. + System.Math.Sin (3.6 * x))
let f20 x : float = 0.1 * (x ** 2.) - x * System.Math.Log x

let f18n x : float = 1. + 1. / (2. * sqrt(x)) + 1. / (3. * System.Math.Cbrt(x * x))
let f19n x : float = 1. + (18. * System.Math.Cos(18. * x / 5.) / (5. * (3. + System.Math.Sin(18. * x / 5.)) * (3. + System.Math.Sin(18. * x / 5.))))
let f20n x : float = 0.2 * x - System.Math.Log x - 1.


let f18i x : float = -sqrt(x) - System.Math.Cbrt (x) + 2.5
let f19i x : float = 1. / (3. + System.Math.Sin(3.6 * x))
let f20i x : float = System.Math.E ** (0.1 * x)

let main =
    printfn "%10.5f  %10.5f  %10.5f" (dichotomy f18 0.4 1.) (iterations f18i 0.7) (newthon f18 f18n 0.7)
    printfn "%10.5f  %10.5f  %10.5f" (dichotomy f19 0. 0.85) (iterations f19i 0.425) (newthon f19 f19n 0.425)
    printfn "%10.5f  %10.5f  %10.5f" (dichotomy f20 1. 2.) (iterations f20i 1.5) (newthon f20 f20n 1.5)

main