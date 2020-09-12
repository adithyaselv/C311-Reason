/* 
 * Lecture 5
 * 
 * Simple lambda calculus interpreter 
 * 
 * Date : 8th September 2020
 * Author : Adithya Selvaprithivraj
 * */


type lambdaExp = 
  | Var(string)
  | Num(int)
  | Lambda(string, lambdaExp)
  | App(lambdaExp, lambdaExp)

type meaning = 
  | Num(int)
  | Closure(meaning => meaning)
  | Error(string)

type environment = string => meaning

let charToNumEnv = (y: string): meaning => {
    switch y {
    | "A" => Num(1)
    | "B" => Num(2)
    | "C" => Num(3)
    | "D" => Num(4)
    | _ => Num(42)
    }
}

let extendEnv = (x: string,v: meaning, env: environment): environment => {
    (y: string) => {
        if(y == x) v else env(y)
    }
}


let rec valof = (exp: lambdaExp, env: environment): meaning  => {
    switch exp {
    | Var(s) => env(s)
    | Num(i) => Num(i)
    | Lambda(s, body) => Closure((y: meaning) => valof(body, extendEnv(s, y, env))) 
    | App(rator, rand) => 
        switch (valof(rator, env)) {
        | Closure(clos) => clos(valof(rand, env)) 
        |_ => Error("Invalid expression")  
        }
    }
}

Js.log(valof(App(Lambda("x", Var("B")), Num(46)), charToNumEnv))
Js.log(valof(App(Lambda("x", Var("x")), Num(46)), charToNumEnv))
Js.log(valof(App(Lambda("x", Var("y")), Num(46)), charToNumEnv))
Js.log(valof(App(App(Lambda("x", Lambda("y", Var("y"))), Num(46)), Num(1729)), charToNumEnv))