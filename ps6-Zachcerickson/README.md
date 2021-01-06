# CSCI 3366 Programming Languages

R. Muller

## Problem Set 6: Type Checking

### Due: Friday March 13, 2020, 6PM

### 12 Points

---

This problem set involves implementing a type checker for the programming language Earth.

### Earth Programs

Earth is an extension of Venus that supports non-recursive product, sum and record types, call-by-value evaluation and recursive functions.  Earth is discussed and described in the writeup found [here](earth.pdf).

The code compiles as is, type:

```bash
> cd
> cd csci3366/ps6-YOURGITHUBID/src/
> dune exec bin/main.exe
Earth> 
```

Your `src/` folder contains a macOS binary `solved`. Type

```bash
> cd src
> ./solved
Earth> 
```

**Cleanup**

```bash
> dune clean
```

Importing definitions from files

```bash
> dune exec bin/main.exe myFile.earth
Earth>                                       // defs from myFile.earth available
```

Several example files are in the `examples/` directory.

**Testing**

```bash
> dune exec bin/main.exe test
```

---

##### Examples

```
Earth> let f(x : int * real) : real = second(x)

Earth> let p : int * real = (3, 1.0 +. 2.14)

Earth> p
value = (3, 3.14) : int * real

Earth> f(p)
value = 3.14 : real

Earth> let fact(n : int) : int = if n = 0 then 1 else n * fact(n - 1)

Earth> fact(4)
value = 24 : int

Earth> let v : int + bool = inl(3, bool)

Earth> case v of inl(x:int) => x + 1 | inr(x:bool) => 0
value = 4 : int

Earth> let g(x:int): int = let f(x:int):int = x + 2 in f(x) + 3

Earth> g(1)
ast = g(1)
value = 6 : int
```

Earth programs can be stored in source files. Here is the file `gcd.earth`

```
let gcd(m : int, n : int) : int =
  if n = 0 then
    m
  else
    gcd(n, m % n)
```

The `src` folder contains an implementation of Earth with the static semantics (**typechecker.ml**) having only a stub.  Implement the type checker as specified in the static semantics in `earth.pdf`.
