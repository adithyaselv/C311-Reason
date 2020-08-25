/* 
 * Lecture 1
 * Natural recursion 
 * 
 * Date : 25th August 2020
 * Author : Adithya Selvaprithivraj
 * */

let rec plus = (a:int , b:int):int => {
    switch b {
    | 0 => a
    | v => 1 + plus(a, v-1)
    };
}

let rec star = (a:int , b:int):int => {
    switch b {
    | 0 => 0
    | v => a + star(a, v-1)
    };
}

let rec exp = (a:int , b:int):int => {
    switch b {
    | 0 => 1
    | v => a * exp(a, v-1)
    };
}

let rec sumList = (l: list(int)): int => {
    switch l {
    | [] => 0
    | [first, ...rest] => first + sumList(rest)
    };
}

let rec listLength = (l: list('a)): int => {
    switch l {
    | [] => 0
    | [_, ...rest] => 1 + listLength(rest)
    };
}

Js.log("Adding 5 + 6 = " ++ string_of_int(plus(5,6)))
Js.log("Multiplying 3 * 5 = " ++ string_of_int(star(3,5)))
Js.log("Exponential 3^5 = " ++ string_of_int(exp(3,5)))
Js.log("List sum [1, 2, 3, 4, 5] = " ++ string_of_int(sumList([1, 2, 3, 4, 5])))
Js.log("Length of list [1, 2, 3, 4, 5] = " ++ string_of_int(listLength([1, 2, 3, 4, 5])))