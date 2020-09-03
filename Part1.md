# Teaching Thursdays

## Plan for the next few weeks
Part 1: Lambda Calculus, a bit of Purescript
Part 2: Types (ADTs)
Part 3: Recursive Types (higher order functions)
Part 4: a bit more theory? type classes?

## Part 1: Lambda Calculus

```
e ::= x | \x. e | e e
```

In order to create a lambda term (expression), we can:
1. write a variable name like x, y, z...
2. write a lambda abstraction like \x. e
3. write a lambda application by juxtaposing two expressions

x <- rule 1
y
z

\x. x <- rule 2 + 1
\x. y
\x. (\y. x)
\x. (\y. (\z. .... y))

x x <- rule 3 + rule 1
(x (x (x (x (x x)))))
(\x. x) y

----------------------------------------------------------------

There are two main operations we care about:
1. alpha equivalence
2. beta reduction

1. Alpha Equivalence

x[x := y]
y

[(\x. x) y z][y := a]
(\x. x) a z

2.Beta Reduction

((\x. x) y)[beta]
(x)[x := y]
y

(\x. (\y. x + y)) 1 2 [beta]
(\y. x + y)[x := 1] 2
(\y. 1 + y) 2 [beta]
(1 + y)[y := 2]
1 + 2
3

(\x. (\y. (\z. x + y + z))) 1 2 3 [beta]
(\y. \z. x + y + z) [x:= 1] 2 3 
(\y. \z. 1 + y + z) 2 3[beta]
(\z. 1 + y + z) [y:= 2] 3 
(\z. 1 + 2 + z) 3 [beta]
(1 + 2 + z) [z:= 3]
1 + 2 + 3
6

(\f. (\x. f x)) (\y. (\z. z)) a [beta]
(\x. f x)[f := (\y. (\z. z))] a
(\x. (\y. (\z. z)) x) a [beta]
((\y. (\z. z)) x)[x := a]
(\y. (\z. z)) a [beta]
(\z. z)[y := a]
(\z. z)

(\x. x x) (\x. x x) [beta]
(x x ) [x :=\x. x x] 
(\x. x x) (\x. x x) [beta]
(x x) [x:= \x. x x]
(\x. x x) (\x. x x)


----------------------------------------

int Add(int a, int b) {
    Console.WriteLine("hello");
    Console.ReadLine();
    throw new Exception("nope, no answer here");
    retun a + b;
}

f : Int x Int -> Int



