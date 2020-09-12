/* 
 * Lecture 4
 * 
 * Bound and free variables in lambda expression
 * 
 * Date : 3rd September 2020
 * Author : Adithya Selvaprithivraj
 * */

type lambdaExp = 
  | Var(string)
  | Lambda(string, lambdaExp)
  | App(lambdaExp, lambdaExp)


  let rec freeuhenv = (sym: string, exp: lambdaExp, env: list(string)): bool => {
      switch exp {
      | Var(s) => s == sym && (! List.mem(s, env))
      | Lambda(s, body) => freeuhenv(sym, body, List.append([s],env))
      | App(rator, rand) => freeuhenv(sym, rator, env) && freeuhenv(sym, rand, env)
      }
  }

  let rec freeuh = (sym: string, exp: lambdaExp): bool => {
    switch exp {
    | Var(s) => s == sym
    | Lambda(s, body) => (s != sym) && freeuh(sym, body)
    | App(rator, rand) => freeuh(sym, rator) && freeuh(sym, rand)
    }
  }

  let rec bound = (sym: string, exp: lambdaExp): bool => {
    switch exp {
    | Var(s) => s != sym
    | Lambda(s, body) => s == sym && !(bound(sym, body))
    | App(rator, rand) => bound(sym, rator) || bound(sym, rand)
    }
  }
  
  let rec boundenv = (sym: string, exp: lambdaExp, env: list(string)): bool => {
    switch exp {
    | Var(s) => s == sym && List.mem(s, env)
    | Lambda(s, body) => boundenv(sym, body, List.append([s],env))
    | App(rator, rand) => boundenv(sym, rator, env) || boundenv(sym, rand, env)
    }
  }

  Js.log(freeuh("x", Lambda("y", App(Var("x"), Var("x")))))
  Js.log(freeuhenv("x", Lambda("y", App(Var("x"), Var("x"))), []))
  Js.log(freeuh("y", Lambda("y", App(Var("x"), Var("x")))))
  Js.log(freeuh("y", Lambda("y", App(Var("y"), Var("x")))))
  Js.log(bound("x", Lambda("y", App(Var("x"), Var("x")))))
  Js.log(bound("x", Lambda("x", App(Var("x"), Var("x")))))
  Js.log(boundenv("x", Lambda("y", App(Var("x"), Var("x"))), []))
  Js.log(boundenv("x", Lambda("x", App(Var("x"), Var("x"))), []))
