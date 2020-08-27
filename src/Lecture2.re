/* 
 * Lecture 2
 * General recursive functions
 * 
 * Ackermann 
 * 
 * Date : 27th August 2020
 * Author : Adithya Selvaprithivraj
 * */

 let rec plus = (x:int, y:int): int => {
     switch y {
     | 0 => x
     | v => 1 + plus(x, v-1) 
     };
 }

 let rec star = (x:int, y:int): int => {
    switch y {
    | 0 => 0
    | v => x + star(x, v-1) 
    };
}

let rec exp = (x:int, y:int): int => {
    switch y {
    | 0 => 1
    | v => x * exp(x, v-1) 
    };
}

let rec doubleExp = (x:int, y:int): int => {
    switch y {
    | 0 => 1
    | v => exp(x, doubleExp(x, v-1))
    }
}

let rec g: int => (int, int) => int = i => {
    (x, y) => {
        switch i {
        | 0 => switch y {
                | 0 => x
                | _ => 1 + g(0)(x, y-1) 
            }
        | 1 => switch y {
                | 0 => 0
                | _ => g(i-1)(x, g(i)(x, y-1))
            }
        | _ => switch y {
                | 0 => 1
                | _ => g(i-1)(x, g(i)(x, y-1))
            }        
        };
    } 
}

let rec map: (list('a), 'a => 'b) => list('b) = (ls, f) => {
    switch ls {
    | [] => []
    | [first, ...last] => [f(first)] @ map(last, f)
    }
} 



Js.log("Double Exp of 2 4 : " ++ string_of_int(doubleExp(2,4)))

Js.log("Addition of 2 4 : " ++ string_of_int(g(0)(2,4)))
Js.log("Multiplication of 2 4 : " ++ string_of_int(g(1)(2,4)))
Js.log("Exponential 2^4 : " ++ string_of_int(g(2)(2,4)))
Js.log("Double Exp using G of 2^^4 : " ++ string_of_int(doubleExp(2,4)))
Js.log("List map example [1,2,3,4] : ")
Js.log(Array.of_list(map([1,2,3,4], x => x*x)))