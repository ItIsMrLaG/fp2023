  $ ./demoInterpret.exe <<- EOF
  > 1 + 2
  3

  $ ./demoInterpret.exe <<- EOF
  > let [a; b] = [1; 2] in a + b
  3

  $ ./demoInterpret.exe <<- EOF
  > let rec len l =
  >   match l with
  >   | [] -> 0
  >   | _ :: tl -> (len tl) + 1
  > in
  > len [0; 1; 2; 3; 4]
  5

  $ ./demoInterpret.exe <<- EOF
  > let sum l =
  >   let rec helper acc l =
  >     match l with
  >     | [] -> acc
  >     | hd :: tl -> helper (acc + hd) tl
  >   in
  >   helper 0 l
  > in
  > sum [0; 1; 2; 3; 4]
  10

  $ ./demoInterpret.exe <<- EOF
  > let id a = a in
  > id (id (id (id 42)))
  42

  $ ./demoInterpret.exe <<- EOF
  > let sum (a, (b, c), d) = a + b + c + d in
  > sum (1, (20, 20), 1)
  42

  $ ./demoInterpret.exe <<- EOF
  > let fac n =
  >   let rec helper n acc =
  >     if n <= 1 then
  >       acc
  >     else
  >       helper (n - 1) (n * acc)
  >   in
  >   helper n 1
  > in
  > fac 5
  120

  $ ./demoInterpret.exe <<- EOF
  > let fib n =
  >   let rec helper a b n =
  >     if n > 0 then
  >       helper b (a + b) (n - 1)
  >     else
  >       a
  >     in
  >   helper 0 1 n
  > in
  > fib 5
  5

  $ ./demoInterpret.exe <<- EOF
  > let fib n [x; y] =  n+x+y in
  > fib 5 [1;2]
  8
CPS Fibonacci
  $ ./demoInterpret.exe <<- EOF
  > let rec fibk n k = if n<2 then k n else fibk (n-1) (fun a -> fibk (n-2) (fun b -> k (a+b)) in
  > fib 5 (fun y -> y)
  Char not found

