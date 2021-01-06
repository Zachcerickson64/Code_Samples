# CSCI 3366 Programming Languages

R. Muller

##Earth

This directory contains OCaml code implementing an interpreter for the mini-PL called Earth. This language extends the mini-PL Venus with recursive functions and non-recursive structured types, products, sums & records. Earth can be seen as a cribbed version of Pascal.

##### Compile and run the Earth REPL

```
> cd src
> dune exec bin/main.exe
Earth> 
```

Cleanup

```bash
> dune clean
```

##### Examples

````
Earth> (true, 2 + 3)
ast = (true, +(2, 3))
value = (true, 5) : (bool * int)

Earth> let r : {a : int; b : bool} = {a = 1 + 2; b = false}

Earth> r.a
ast = r.a
value = 3 : int

Earth> let sum : int + real = inl(2 + 3, real)

Earth> sum
ast = sum
value = inl(5, real) : (int + real)

Earth> case sum of inl(x : int) => x * 2 | inr(x : real) => 300
ast = case sum of inl(x:int) => *(x, 2) | inr(x:real) => 300
value = 10 : int

Earth> quit
>
````

##### Earth Code in Source Files

Evaluate top-level definitions in source files

```bash
> dune exec bin/main.exe myfile.earth
```

There are a few examples in `examples/`. Euclid's gcd algorithm in a file such as `gcd.earth`:

```
let gcd(m : int, n : int) : int =
  if n = 0 then
    m
  else
    gcd(n, m % n)
```

Babylonian approximation of square roots

````
let abs(r : real) : real = (if r <. 0.0 then ((0.0 -. 1.0) *. r) else r)

let sqrt(r : real) : real =
  let error : real = 0.01 in
  let loop(lo : real, hi : real, n : int) : real =
    let guess : real = (lo +. hi) /. 2.0
    in
    if n = 0 then
      0.0
    else
      let gs : real = guess *. guess
      in
      if abs(r -. gs) <. error then
        guess
      else
        if (gs >. r) then
          loop(lo, guess, n - 1)
        else
          loop(guess, hi, n - 1)
  in
  loop(0.0, r, 40)
````

Primality testing

````
let isFactor(m : int, n : int) : bool = (n % m = 0)

let isPrime(n : int) : bool =
  let top : int = r2i(sqrt(i2r(n))) in
  let loop(i : int) : bool =
    if i > top then
      true
    else
      if isFactor(i, n) then
        false
      else
        loop(i + 1)
  in
  loop(2)
````

**Testing**

```bash
> dune exec bin/main.exe test
```

