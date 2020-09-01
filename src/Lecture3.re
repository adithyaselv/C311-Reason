/* 
 * Lecture 3
 * Match and L0
 * Switch has pattern matching capabilities (so similar to match)
 * 
 * Date : 1st September 2020
 * Author : Adithya Selvaprithivraj
 * */


 type l0 =
   | Plux(l0, l0)
   | Monus(l0, l0)
   | Exp(int);

/*
check if there is reason any type
let rec l0uh = (exp: 'a): bool => {
    switch exp {
    | Plux(exp1, exp2) => l0uh(exp1) && l0uh(exp2)
    | Monus(exp1, exp2) => l0uh(exp1) && l0uh(exp2)
    | Exp(_) => true
    | _ => false
    }
}     */

let rec valof = (exp: l0): int => {
    switch exp {
    | Plux(exp1, exp2) => valof(exp1) + valof(exp2)
    | Monus(exp1, exp2) => valof(exp1) - valof(exp2)
    | Exp(num) => num
    }
} 


Js.log("L0 test program 1 Plux(Monus(Exp(10), Exp(3)), Exp(8)) " ++ string_of_int(valof(Plux(Monus(Exp(10), Exp(3)), Exp(8)))))

/* Js.log("L0 test program 2 Plux(Monus(Exp(10), Exp(3)), Exp(8)) ")
Js.log(l0uh("sasads")) */