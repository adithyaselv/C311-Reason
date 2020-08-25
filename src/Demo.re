
let rec fact = (n : int): int => {
    switch n {
    | 0 => 1
    | v => v * fact(v-1)
    };
}

Js.log("Hello, BuckleScript and Reason!");
Js.log("Factorial of 5 is " ++ string_of_int(fact(5)));
