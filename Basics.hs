{-
---
fulltitle: Haskell Basics
date: September 7, 2022
---

Welcome to Haskell!
-------------------

If you are reading this text online, you may wish to access it as a Haskell
project instead. You can find this module as part of the repository
[01-basics](https://github.com/upenn-cis5520/01-basics) on github. We have prepared
instructions on using
[github with CIS 5520](https://www.cis.upenn.edu/~cis5520/current/version.html)
and with installing
[GHC and VSCode](https://www.cis.upenn.edu/~cis5520/current/haskell-vscode.html).
We strongly encourage you to read this file in the VSCode editor so that you
can experiment with it.

Every Haskell file begins with a few lines naming the module (this name must
start with a capital letter and be the same as the file name) and (optionally)
importing definitions from other modules.
-}

module Basics where

-- library imports must come at the beginning
import Test.HUnit
  ( Counts,
    Test (TestList),
    runTestTT,
    (~?),
    (~?=),
  )
import Prelude hiding (const, sum, take)

{-
Observe that Haskell supports two kinds of comments: single line comments
start with `--` and block comments begin and end with `{-` and `-}`
respectively.

Understanding a Haskell program is about equality
-------------------------------------------------

*Functional* programming means that the semantics of a program can be
described mathematically. One principle of mathematics is called Leibniz
equality: in any context, we can replace an object with anything equivalent
to it.  Therefore, in Haskell, we reason about computation by reasoning about
*equality* of (sub-)expressions.

For example, if we want to know what value an arithmetic expression computes
to, we only need to find some number that is equal to it.

    3 * (4 + 5)

    { 4+5 is equal to 9 by addition, so we can replace it in the expression }

    3 * 9

    { by multiplication }

    27

That's it!

Furthermore, we can ask VSCode to compute the value of an expression for us with a
special form of comment (i.e. a single line comment that starts with '>>>').

Try clicking on "Evaluate..." below.
-}

-- >>> 3 * (4 + 5)

{-
A Haskell module (like this one) is a list of *definitions*. These definitions
allow us to give names to Haskell expressions.
-}

ex :: Integer
ex = 3 * (4 + 5)

{-
We can ask VSCode to calculate these values, just as we did above.
-}

-- >>> ex

{-
Whenever we give a name to an expression, it is a good idea to also write down
its type. Although Haskell can often figure out this type for you, when you
are just starting out, the error messages can be perplexing without these
additional hints.

The `Integer` type is the type of arbitrarily large integers in Haskell.
-}

bigInteger :: Integer
bigInteger = 12345678901234567890

{-
This is in contrast to the `Int` type, for word-sized integers (machine
dependent). Numbers are overloaded in Haskell, so the type annotation tells
the compiler how to interpret this expression. (And, note the warning
issued by the compiler for this out of range number!)
-}

bigInt :: Int
bigInt = 12345678901234567890

{-
Compare the value of a extra-large `Integer`
-}

-- >>> bigInteger

{-
with an `Int`
-}

-- >>> bigInt

{-
Above, we declared the type of an expression separately from giving it a
name. However, if we don't want to give a name to an expression, we can
still annotate it with its type using `::`.
-}

-- >>> 31 * (42 + 56) :: Integer

{-
More generally, the type annotation can be attached to any subexpression, not
just at the top level.
-}

-- >>> (31 :: Integer) * (42 + 56)

{-
It is good style to annotate the type of *every* declaration in a Haskell
program. This helps with error messages, as Haskell operators, like `*`, and
constants like '31', are often overloaded.

Elements of Haskell
-------------------

So far, we have have seen the following three properties of Haskell:

* Haskell code is based on *expressions*
* Expressions evaluate to *values*
* Every expression has a *type*, which may influence evaluation

You are probably familiar with expressions in other programming languages,
where they are often used to compute numeric and boolean values. Haskell also
includes these types and operators.

For example, Haskell includes floating point numbers, via the `Double` type,
using the same overloaded syntax.
-}

-- >>> 31 * (42 + 56) :: Double    -- double precision floating point

{-
Furthermore, you'll also find characters, strings and boolean values.
-}

-- >>> 'a' :: Char                 -- characters

-- >>> "abcd" :: String            -- strings

-- >>> "cis" ++ "552"              -- string concatenation

-- >>> True :: Bool                -- boolean values

-- >>> 1 <= 3 || False && 3 > 2    -- boolean operators, comparisons

{-
What is a little different about Haskell is that everything is an expression,
including conditionals. This means that `if` can be nested inside other
expressions.
-}

-- >>> (if ex > 28 then 1 else 0) + 2 :: Int

{-
Now the last basic type, shown below, is subtle. It is a special constant,
written `()` and called "unit". The type of this constant is written with the
same notation, and also called "unit". You'll need to pay attention to
context to know whether we mean the constant or the type. The key fact about
this basic type is that there is only *one* value with type `()`.
-}

-- >>> () :: ()            -- 'unit' (both value and type have the same syntax)

{-
What is Abstraction?
--------------------

We're going to talk a lot about *abstraction* in this course, starting from
simple examples and getting dramatically more expressive. But what is
abstraction, exactly?

The first answer is: "Pattern Recognition"

    31 * (42 + 56)

    70 * (12 + 95)

    90 * (68 + 12)

What do these expressions have in common? They all follow the same general
pattern. We can generalize that pattern to a *function* by defining an equation.
-}

pat :: Integer -> Integer -> Integer -> Integer
pat a b c = a * (b + c)

{-
We call functions by providing them with arguments.
-}

-- >>> pat 31 42 56

{-
No parentheses are necessary, unless the argument itself is a compound expression.
-}

-- >>> pat (30 + 1) 42 56

{-
The important question is not "What does this function do?"
but, instead "What does this function mean?" We can reason
about that meaning using what we know about equality.

    pat 31 42 56

    { function call, replace a b & c in right-hand side of equation by 31 42 and 56 }

    == 31 * (42 + 56)

    { addition }

    == 31 * 98

    { multiplication }

    == 3038

Functions, like `pat`, are the core abstraction mechanisms in functional
programming.

Function types
--------------

Like all expressions, functions have types.

The type of a function taking an input of type `A` and yielding an output of
type `B` is written as

    A -> B

For example, the `pos` function determines whether an `Int` is strictly
greater than zero.
-}

pos :: Int -> Bool
pos x = x > 0

{-
The `pat` function above takes multiple arguments. Therefore, its type has
multiple `->`s, one for each argument.

The type of a function taking inputs of type `A1`, `A2`, and `A3` and
returning a result of type `B` is written as

    A1 -> A2 -> A3 -> B

Symbolic vs. alphabetic names
-----------------------------

Symbolic identifiers (i.e. `+` and `*`) are infix by default.

Parentheses around a symbolic name turn it into a regular
name.

For example, if we want to define an alphabetic name for the
addition function, we can do so.
-}

plus :: Int -> Int -> Int
plus = (+)

{-
And we can call operations in parentheses just like "standard" functions, by
writing their arguments afterwards.
-}

p0 :: Int
p0 = (+) 2 4

{-
Likewise we can use alphabetic name in backquotes as infix.
-}

p1 :: Int
p1 = 2 `plus` 2

{-
Laziness is a virtue
--------------------

One major difference between Haskell and other programming languages is that
Haskell uses a "call-by-need" semantics for evaluation, aka "lazy" evaluation.
What this means is that Haskell does not evaluate arguments before calling
functions. Instead, expressions are only evaluated when they are needed.

We can observe this behavior in Haskell by seeing what happens when we use
`error` in a subexpresion. The `error` keyword in Haskell triggers a
non-recoverable runtime exception, aborting any computation in progress.
An `error` can be used in any context and can be given
any type because it does not produce a value.
If an error is triggered, then we know that subexpression was evaluated.

For example, addition always needs to evaluate its arguments, so this
error will trigger.
-}

-- >>> 1 + 2 + 3 + error "Here!"

{-
However, we won't trigger an error that is in dead code, such as in
the non-selected part of an if-expression...
-}

-- >>> if 1 < 3 then 5 else error "Unreachable"
-- 5

{-
..or that was short-circuited when evaluating a boolean expression.
-}

-- >>> True || error "Unreachable"
-- True

{-
In contrast, you can see that if the first argument were `False` instead,
it does not short circuit and does not trigger the error.
-}

-- >>> False || error "Ooops!"
-- Ooops!

{-
In most languages, `if` and `||` are defined via special constructs because they
include sub-expressions that are not always evaluated. However, in Haskell, these
constructs are less special. For example, you can define your own short-circuiting
version of the `or` operator. Suppose you would like this operator to be written
with three pipes instead of two:
-}

(|||) :: Bool -> Bool -> Bool
(|||) a b = if a then True else b

{-
Through laziness, this definition short circuits, just like the Prelude version of `||`.
-}

-- >>> True ||| error "Unreachable"
-- True

{-
More generally, because Haskell is lazy, the language enables more abstraction.
Functions and operators that we define can have nontrivial control behavior.

Laziness is also the reason that we can reason about Haskell programs just by thinking
about equalities. For example, there is a function in the Prelude with the following
definition:
-}

const :: a -> b -> b
const x y = y

{-
In a call-by-value language (i.e. most languages) if you see a subexpression like
`const (f 3) 4`, you have to know whether the expression `f 3` produces a normal value
first before you can know that the result is 4. However, in Haskell, you can use
substitution: the pattern above says that with any call to `const`, the result is the value of
the second argument.

Thus:
-}

-- >>> const (error "Here!") 4
-- 3

{-
We'll see more examples of laziness throughout the semester. Sometimes we use the word "strictness" to
describe how functions use their arguments. If an argument will always be evaluated before the
function is called, we call this argument "strict." For example, both arguments in an addition
operation are strict, because we need to know what the numbers are to sum them together. However,
only the first argument in `||` is strict because it does not evaluate the second argument when
the value of the first one is `True`.

Making Haskell DO something
===========================

Programs often interact with the world:

* Read files
* Display graphics
* Broadcast packets
* Run test cases and print success or failure

They don't *just* compute values.

How does this fit with values & equalities above?

Note, we've gotten far without doing any I/O. That's fairly standard in
Haskell. Working with VSCode means that we can see the answers directly, we
don't need an action to print them out. However, a standalone executable needs
to do *something*, so we demonstrate that next.

The GHC System
--------------

We'll start with a few examples just using the interactive toplevel for the
Haskell language. Although Haskell is a compiled language, the interactive
toplevel, "ghci" is available for experimentation. You can access this
toplevel using any command prompt (i.e. Terminal), as long as you have GHC
installed. The examples below also assume that you have the "stack" tool
available and that you have started the command prompt in the same directory
that contains this source code. [Instructions for installing "stack" and
other tools are available.](https://www.cis.upenn.edu/~cis5520/current/haskell-vscode.html)

First use the terminal to start `ghci` and instruct it to load the `Basics`
module.

    sweirich@sixteen 01-basics % stack ghci Basics.hs
    Using configuration for cis5520-basics:lib to load /Users/sweirich/552/cis5520-20fa/lectures/01-basics/Basics.lhs
    cis5520-basics> configure (lib)
    Configuring cis5520-basics-0.1.0.0...
    cis5520-basics> initial-build-steps (lib)
    Configuring GHCi with the following packages: cis5520-basics
    GHCi, version 8.8.3: https://www.haskell.org/ghc/  :? for help
    [1 of 1] Compiling Basics           ( /Users/sweirich/5520/cis5520-22fa/lectures/01-basics/Basics.lhs, interpreted )
    Ok, one module loaded.
    Loaded GHCi configuration from /private/var/folders/p3/pkythxvx6rq9q054797y4bb80000gn/T/haskell-stack-ghci/48e82592/ghci-script
    *Basics>

Now, with the prompt from the module, you can ask for the values, types and
information of expressions.

    *Basics> ex
    27
    *Basics> :t ex
    ex :: Integer
    *Basics> :i ex
    ex :: Integer     -- Defined at Basics.hs:5:1

Obligatory Hello World
----------------------

"IO actions" are a new sort of sort of value that describe an effect
on the world.

    IO a  --  Type of an action that returns an `a`  (a can be anything!)

Actions that *do* something but return nothing have the type `IO ()`.

    putStr :: String -> IO ()

So `putStr` takes in a string and returns an *action* that writes the string
to stdout.

Because `putStr` doesn't evaluate to a value that we can print, we can't play with
it using the IDE. Even if you hit `Evaluate...` you will see nothing because the
IDE hides the printing action and only displays the value.
-}

-- >>> putStr "Say what?"

{-
Instead, to observe the printing action, we need to use GHCi. Let's give it
name first:
-}

hw :: IO ()
hw = putStr "Hello World!\n"

{-
and then we can give it a try.

    *Basics> hw
    Hello World!
    *Basics>

Just 'do' it
-------------
How can we do many actions?  By composing small actions.

The `do` syntax allows us to create a compound action that sequences one
action after another. The definition of `many` below is a compound action
that outputs the three strings in order. (Try it out in ghci!)
-}

many :: IO ()
many = do
  putStr "Hello" -- each line in the sequence
  putStr " World!" -- must be an IO action
  putStr "\n" -- don't forget the newline

{-
Note: white-space is significant here. The `do` notation sequences actions, but
each action in the sequence must start at the same character offset: all of
the `putStr`s must be lined up.

Sometimes people put the `do` on a line by itself and then start the list
of actions on the next line. This saves column width in larger developments.
-}

many' :: IO ()
many' = do
  putStr "Hello"
  putStr " World!"
  putStr "\n"

{-
Example: Input Action
---------------------

Actions can also return a value.

    getLine :: IO String

This action reads and returns a line from stdin.  We can name the result
as part of a `do` sequence, with this notation

    x <- action

Here `x` is a variable that can be used to refer to the result of the
action in later code.
-}

query :: IO ()
query = do
  putStr "What is your name? "
  n <- getLine
  let y :: String
      y = "Welcome to CIS 552 " ++ n
  putStrLn y

{-
When we sequence actions of type `IO ()` there is no need to name the
result. These actions do not return anything interesting. We could name the
result if we wanted (such as `_m` below); but because of its type we know
that `_m` will always be a special value, written `()` and called "unit". By
convention, the name of an unused variable starts with an underscore.
-}

query' :: IO ()
query' = do
  _m <- putStr "What is your name? "
  n <- getLine
  putStrLn ("Welcome to CIS 552 " ++ n)
  _st <- query2
  return ()

{-
Note that you cannot name the *last* action in a sequence. Names are there so that
you can use their results later. If you want to return the value instead, the last action
should be a `return`.
-}

query2 :: IO String -- compare this type to `query` above.
query2 = do
  putStr "What is your name? "
  n <- getLine
  return n

{-
Furthermore, there is no need to name a value if it is just going to be
returned right away. This version is equivalent.
-}

query2' :: IO String
query2' = do
  putStr "What is your name? "
  getLine

{-
Example: Testing Actions
------------------------

The `hunit` library contains definitions for constructing unit tests for your
programs. You must import this library at the top of your module (with
import Test.HUnit`) before you can access these definitions.  This library
defines a `Test` type for test cases.
-}

t1 :: Test
t1 = (1 + 2 :: Int) ~?= 3

-- check that the result of the computation matches the expected value `3`

{-
* Haskell is lazy, so these definitions *create* tests, but don't actually run
them yet. We'll do that below.

* The `(~?=)` operator is overloaded. You can create tests that compare
expressions at many different types. When the expressions themselves are also
overloaded (such as those with numbers), we run into ambiguity---what type of
expressions should this test actually use? We resolve that ambiguity with
an internal typing annotation `(3 :: Int)`.

To run the test case, we use the function `runTestTT`.

           runTestTT :: Test -> IO Counts
-}

numTest :: IO Counts
numTest = runTestTT t1

{-
This expression is an action that runs the test case(s) and returns a data
structure (of type `Counts`) recording how many tests cases were run and how
many failed or produced errors. A failing test case is caused by a false
assertion, for example `True ~?= False`. An erroroneous testcase is one that
produced a run-time error, such as division by zero.

Although we can evaluate `numTest` in the IDE, we get more information about
failing and erroroneous tests using ghci.

          *Basics> runTestTT (True ~?= False)
          ### Failure:
          <interactive>:16
          expected: False
          but got: True
          Cases: 1  Tried: 1  Errors: 0  Failures: 1
          Counts {cases = 1, tried = 1, errors = 0, failures = 1}

          *Basics> runTestTT ( 1 `div` 0 ~?= (1 :: Int))
          ### Error:
          divide by zero
          Cases: 1  Tried: 1  Errors: 1  Failures: 0
          Counts {cases = 1, tried = 1, errors = 1, failures = 0}

Structured Data
===============

Tuples
------

An ordered sequence of values is called a *tuple* and its type is written as
(where `A1` ... `An` are the types of the values in the sequence).

        (A1, ..., An)

For example,
-}

tup1 :: (Char, Int)
tup1 = ('a', 5)

tup2 :: (Char, Double, Int)
tup2 = ('a', 5.2, 7)

tup3 :: ((Int, Double), Bool)
tup3 = ((7, 5.2), True)

{-
There can be any number of elements in a tuple, but the structure must match
the type.  (Actually, the compiler won't let you construct a tuple with more
than 62 values, but it would be bad style to do so anyways.)

*Pattern Matching* extracts values from tuples.

A function that takes a tuple as an argument *looks* like it has multiple
arguments, but in reality, it has just one. We use a *pattern* to name the
three components of the tuple for use in the function.
-}

tpat :: (Int, Int, Int) -> Int
tpat (a, b, c) = a * (b + c)

{-
We can put *anything* in a tuple
--------------------------------

We can have tuples of tuples. These three values below have three different
types. Look closely!  (We use the word 'pair' to describe a tuple with two
values.)
-}

tup4 :: ((Int, Int), Int)
tup4 = ((1, 2), 3) -- a pair of a pair and a number

tup5 :: (Int, (Int, Int))
tup5 = (1, (2, 3)) -- a pair of a number and a pair
{-

-}

tup6 :: (Int, Int, Int)
tup6 = (1, 2, 3) -- a three-tuple, or triple

{-
Note that the pattern that names the variables must match the structure of the
type *exactly*.
-}

pat4 :: ((Int, Int), Int) -> Int
pat4 ((a, b), c) = a * (b + c)

-- >>> pat4 tup4

pat5 :: (Int, (Int, Int)) -> Int
pat5 (a, (b, c)) = a * (b + c)

-- >>> pat5 tup5

pat6 :: (Int, Int, Int) -> Int
pat6 (a, b, c) = a * (b + c)

-- >>> pat6 tup6

{-
We can stick anything in tuples, even IO actions.
-}

act2 :: (IO (), IO ())
act2 = (putStr "Hello", putStr "Hello")

{-
This doesn't actually run both actions, it just creates a pair holding two IO
computations. Haskell doesn't evaluate the components of a tuple when it is
constructed. It waits until you actually need it.

Compare the difference between these definitions in ghci. Try to predict what
they will do.
-}

runAct2 :: IO ()
runAct2 = do
  let (x, y) = act2 -- pattern match in `do` sequences using `let`
  x -- run the first action
  y -- then run the second

runAct2' :: IO ()
runAct2' = do
  let (x, y) = act2 -- pattern match
  y -- run the second action
  x -- then run the first

runAct2'' :: IO ()
runAct2'' = do
  let (x, y) = act2 -- pattern match
  x -- run the first action
  x -- then run it again!

{-
Optional values
---------------

The type of "optional" or "partial" values is written

        Maybe A

This type describes either a value of type `A`, or else nothing.
-}

m1 :: Maybe Int
m1 = Just 2 -- the 'Just' tag tells the compiler that we have a value

m2 :: Maybe Int
m2 = Nothing -- the 'Nothing' tag means there is no value

{-
There is no 'null' in Haskell. Yay!

Extracting values from 'Maybe's
--------------------------------

Pattern Matching extracts values from maybes; we need a pattern for each
case.
-}

pat'' :: Maybe Int -> Int
pat'' (Just x) = x
pat'' Nothing = 2

{-
Patterns can be nested, too.
-}

jn :: Maybe (Maybe a) -> Maybe a
jn (Just (Just x)) = Just x
jn (Just Nothing) = Nothing
jn Nothing = Nothing

{-
**Quiz**: See if you can come up with a slightly simpler way to write `jn` using two
patterns instead of three. The `undefined` expression is one that produces a run-time
error if it is ever evaluated.
-}

jn' :: Maybe (Maybe a) -> Maybe a
jn' = undefined

{-
'Maybe' is useful for partial functions
---------------------------------------
-}

location :: String -> Maybe String
location "cis501" = Just "Wu & Chen"
location "cis502" = Just "Heilmeier"
location "cis520" = Just "Wu & Chen"
location "cis5520" = Just "3401 Walnut, 401B"
location _ = Nothing -- wildcard pattern, matches anything

{-
Lists
=====

The type of a list of values, each of type `A` is written

       [A]

A list is a sequence of values of the same type. There is no limit to
the number of values that can be stored in a list. We notate lists as
a sequence of comma-separated values inside square brackets.
-}

l1 :: [Double]
l1 = [1.0, 2.0, 3.0, 4.0]

l2 :: [Int]
l2 = undefined -- make a list of numbers

{-
Lists can contain structured data...
-}

l3 :: [(Int, Bool)]
l3 = [(1, True), (2, False)]

{-
...and can be nested:
-}

l4 :: [[Int]]
l4 = undefined -- make a list of lists

{-
List elements *must* have the same type.
-}

-- l5 :: [Int]
-- l5 = [ 1 , True ]  -- doesn't type check

{-
(Observe the type error that results when you uncomment the definition above.)

The empty list is written `[]` and pronounced "nil".
-}

l6 :: [a]
l6 = []

{-
*Note*: `String` is just another name for a list of characters (`[Char]`).
-}

l7 :: String
l7 = ['h', 'e', 'l', 'l', 'o', ' ', '5', '5', '2', '!']

{-
What is the value of l7?
-}

-- >>> l7
--

{-
"Cons"tructing Lists
---------------------

The infix operator `:` constructs a new list, by adding a new element to the
front of an existing list. (Note, the existing list is not modified.)
We call this operator `cons`.

(The `a` in the type of `cons` below means that this function works for lists
containing *any* type of element. In other words, we say that this function
is *polymorphic*. More on this later.)
-}

cons :: a -> [a] -> [a]
cons = (:)

c1 :: [Bool]
c1 = True : [False, False]

c2 :: [Int]
c2 = 1 : []

{-
Try evaluating `c1` and `c2`.
-}

-- >>> c1
--
-- >>> c2
--

{-
And check out the type of `c3`.
-}

c3 = [] : []

{-
Strings
-------

The `String` type in Haskell is syntactic sugar for a list of characters. Haskell
doesn't distinguish between the types `[Char]` and `String`. For example, here
are two different ways to write the same list:
-}

s1 :: [Char]
s1 = "abc"

{-
>
-}

s2 :: String
s2 = ['a', 'b', 'c']

{-
Try evaluating `s1` and `s2`.
-}

-- >>> s1
--
-- >>> s2
--

{-
Syntactic Sugar for lists
--------------------------

Haskell views the notation

    [x1, x2, .. , xn]

as short for

    x1 : x2 : .. : xn : []

This means that we can think of lists as a sequence of cons'ed elements,
ending with nil.  For example,

    [1,2,3,4]  and  1 : 2 : 3 : 4 : []

are the same list.
-}

-- >>> [1,2,3,4] == 1:2:3:4:[]
-- True

{-
Function practice: List Generation
----------------------------------

**Example**: Write a function that, given an argument `x` and a number `n`, returns
a list containing `n` copies of `x`.

**Step 1**: Write test cases for the function.

We're using HUnit, a library for defining unit tests in Haskell and using the
`~?=` operator to construct unit `Test`s by comparing the computed result
(first argument) with an expected result (second argument).
-}

testClone1, testClone2, testClone3, testClone4 :: Test
testClone1 = clone 'a' 4 ~?= ['a', 'a', 'a', 'a']
testClone2 = clone 'a' 0 ~?= []
testClone3 = clone (1.1 :: Double) 3 ~?= [1.1, 1.1, 1.1]
testClone4 = clone 'a' (-1) ~?= []

{-
**Step 2**: Declare the type of the function.

This function replicates any type of value, so the type of the first argument
is polymorphic.
-}

clone :: a -> Int -> [a]
{-
**Step 3**: Implement the function.

We implement this function by recursion on the integer argument.
-}

clone x n = if n <= 0 then [] else x : clone x (n -1)

{-
**Step 4**: Run the tests.

The HUnit function `runTestTT` actually runs a given unit test and prints
its result to the standard output stream. (That is why its result type is `IO
Counts`. The IO in the type means that this computation does IO.)
-}

cl1, cl2, cl3, cl4 :: IO Counts
cl1 = runTestTT testClone1
cl2 = runTestTT testClone2
cl3 = runTestTT testClone3
cl4 = runTestTT testClone4

{-
or
-}

cls :: IO Counts
cls = runTestTT (TestList [testClone1, testClone2, testClone3, testClone4])

{-
You can run the tests by evaluating the definition `cls` at the ghci prompt.

    ghci> cls
    Cases: 4  Tried: 4  Errors: 0  Failures: 0
    Counts {cases = 4, tried = 4, errors = 0, failures = 0}

Function practice
-----------------

**Quiz**: Define a function called `range` that, given two integers `i` and `j`,
returns a list containing all of the numbers at least as big as `i` but no
bigger than `j`, in order.

**Step 1**: Write test cases. We can define multiple test cases at once using a list.
-}

testRange :: Test
testRange =
  TestList
    [ range 3 6 ~?= [3, 4, 5, 6],
      range 42 42 ~?= [42],
      range 10 5 ~?= []
    ]

{-
**Step 2**: Declare the type of the function.
-}

range :: Int -> Int -> [Int]
{-
**Step 3**: Define the function. This part is for you to do for your quiz.
-}

range i j = undefined

{-
**Step 4**: Run the tests.
-}

runRTests :: IO Counts
runRTests = runTestTT testRange

{-
Pattern matching with lists
---------------------------

The examples so far have *constructed* various lists. Of course, sometimes
we would like to write functions that *use* lists. We can use a list by
pattern matching...
-}

isHi :: String -> Bool
isHi ['H', 'i'] = True
isHi _ = False

{-
If we have a list of characters, we can use string constants as patterns too.
-}

isGreeting :: String -> Bool
isGreeting "Hi" = True
isGreeting "Hello" = True
isGreeting "Bonjour" = True
isGreeting "Guten Tag" = True
isGreeting _ = False

{-
We can also work with lists more abstractly, for example determining if we have
a list of length one...
-}

isSingleton :: [a] -> Bool
isSingleton [_] = True
isSingleton _ = False

{-
...or of length greater than two. Complete this one yourself using a pattern to identify
lists that have three or more elements.
-}

isLong :: [a] -> Bool
isLong = undefined

testIsLong :: Test
testIsLong =
  TestList
    [ not (isLong []) ~? "nil", -- can convert booleans to tests by naming them via `~?`
      not (isLong "a") ~? "one",
      not (isLong "ab") ~? "two",
      isLong "abc" ~? "three"
    ]

{-
Finally, all of the patterns we have shown you so far have been part of definitions. We can define a function, like `isSingleton` or `isGreeting` above, by cases, using multiple lines. This style is common in Haskell, but if you are coming from OCaml, you might be more familiar with `match`, or a separate expression form of pattern matching. Such a form is also available in Haskell, using the `case` and `of` keywords.

For example, we can rewrite `isGreeting` using `case` instead.
-}

isGreeting2 :: String -> Bool
isGreeting2 s =
  case s of
    "Hi" -> True
    "Hello" -> True
    "Bonjour" -> True
    "Guten Tag" -> True
    _ -> False

{-
Note that all of the patterns in a case expression must start in the same column of your source file. This expression is layout sensitive, and if things don't line up, you will get a compilation error. Case expressions are particularly good for *nested* patterns, where you might want to match again inside of a branch. Here's a silly definition of `isGreeting` that demonstrates a nested pattern. Note how the layout determines where the patterns for the inner `case` end and the ones for the outer `case` resume.
-}

isGreeting3 :: String -> Bool
isGreeting3 s =
  case s of
    ('H' : r) -> case r of
      "i" -> True
      "ello" -> True
      _ -> False
    "Bonjour" -> True
    "Guten Tag" -> True
    _ -> False

{-
Function practice: List Recursion
------------------------------

**Example*: Define a function called `listSum` that, given a list of `Int`s returns
their sum.

**Step 1**: Write test cases.
-}

sumTests :: Test
sumTests =
  TestList
    [ sum [1, 2, 3] ~?= 6,
      sum [] ~?= 0
    ]

{-
**Step 2**: Declare the type of the function.
-}

sum :: [Int] -> Int
{-
**Step 3**: Define the function. (Use pattern matching to define the function by
case analysis.)
-}

sum [] = 0
sum (x : xs) = x + sum xs

{-
**Step 4**: Run the tests.
-}

runSumTests :: IO Counts
runSumTests = runTestTT sumTests

{-
Note that `listAdd` follows a general pattern of working with lists called *list
recursion*.  We can define lists as follows.

A list is either

- `[]`, the empty list, or
- `x : xs`, an element `x` cons'ed onto another list `xs`.

This is a recursive definition, as we are defining the notion of lists in terms
of itself.  Recursive functions that work with lists will follow the
pattern of this definition:

    f :: [a] -> ...
    f [] =  ...       -- case for the empty list
    f (x : xs) = ...  -- case for a nonempty list, will use `f xs`
                      -- recursively somehow.

Function practice: List access
------------------------------

Define a function, called `take`, that, given a number n and a list,
returns the first n items in the list, or the whole list if there are
fewer than n items.

**Step 1**: Write test cases.
-}

takeTests :: Test
takeTests =
  TestList
    [ take 1 [1, 2, 3] ~?= [1],
      take 5 [1, 2, 3] ~?= [1, 2, 3]
    ]

{-
**Step 2**: Declare the type of the function. This function
is polymorphic and works with any element type.
-}

take :: Int -> [a] -> [a]
{-
**Step 3**: Define the function.
-}

take 0 xs = []
take n [] = []
take n (x : xs) = x : take (n - 1) xs

{-
**Step 4**: Run the tests.
-}

runTakeTests :: IO Counts
runTakeTests = runTestTT takeTests

{-
Function practice: List transformation
------------------------------------

Define a function, called `listIncr`, that, given a list of ints,
returns a new list where each number has been incremented.

**Step 1**: Write test cases.
-}

listIncrTests :: Test
listIncrTests =
  TestList
    [ listIncr [1, 2, 3] ~?= [2, 3, 4],
      listIncr [42] ~?= [43]
    ]

{-
**Step 2**: Declare the type of the function.
-}

listIncr :: [Int] -> [Int]
{-
**Step 3**: Define the function.
-}

listIncr = undefined

{-
**Step 4**: Run the tests.
-}

runLITests :: IO Counts
runLITests = runTestTT listIncrTests

{-
Function practice: Double List transformation
---------------------------------------------

Define a function, called `listAdd`, that, given two lists of
numbers, adds them together pointwise. Any extra numbers are ignored.

**Step 1**: Write test cases.
-}

listAddTests :: Test
listAddTests =
  TestList
    [ listAdd [1, 2, 3] [2, 4, 5] ~?= [3, 6, 8],
      listAdd [42] [] ~?= []
    ]

{-
**Step 2**: Declare the type of the function.
-}

listAdd :: [Int] -> [Int] -> [Int]
{-
**Step 3**: Define the function.
-}

listAdd = undefined

{-
**Step 4**: Run the tests.
-}

runLAddTests :: IO Counts
runLAddTests = runTestTT listAddTests

-- >>> runLAddTests
-- Counts {cases = 2, tried = 2, errors = 0, failures = 0}

{-
Function practice: "Infinite" lists
-----------------------------------

The `:` operator is lazy in Haskell. When we create a list we don't need to
know the value of all of the elements.
-}

-- >>> take 2 (1 : 2 : 3 : error "unreachable")
-- [1,2]

{-
Because `:` is lazy, we can define lists in terms of themselves. This list
has as many ones as you want it to contain.
-}

ones :: [Int]
ones = 1 : ones

{-
Do NOT try to print this list.
-}

-- >>> ones

{-
But, we can work with any finite prefix of the list without trouble.
-}

-- >>> take 17 ones
-- [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]

{-
And, we can use this technique to define many different series of
numbers. For example, we can define a simple incrementing series.
-}

allNums :: [Int]
allNums = 1 : listIncr allNums

-- >>> take 17 allNums
-- [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17]

{-
And we can define the Fibonacci series.
-}

fibs :: [Int]
fibs = 1 : 1 : listAdd fibs (tail fibs)

-- >>> take 17 fibs
-- [1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597]

{-
How do you reason about definitions such as `fibs` above? The
technique that we've seen above, replacing equal subterms by equal
subterms works!

```
fibs = -- definition above
      1 : 1 : listAdd fibs (tail fibs)
       -- name a subexpression
     == 1 : 1 : l1
        where l1 = listAdd fibs (tail fibs)
                    -- unfold definition of fibs (twice) to simplify this subexpression
                 == listAdd (1 : 1 : listAdd fibs (tail fibs)) (tail (1 : 1 : listAdd fibs (tail fibs)))
                    -- replace `tail` with its definition
                 == listAdd (1 : 1 : listAdd fibs (tail fibs)) (1 : listAdd fibs (tail fibs))
                    -- replate `listAdd` with its definition, in the case of nonempty lists
                 == 1 + 1 : listAdd (1 : listAdd fibs (tail fibs)) (listAdd fibs (tail fibs))
                    -- add numbers, replace subterms equivalent to l1
                 == 2 : listAdd (1 : l1) l1
       -- replace l1 with simpler version
     == 1 : 1 : 2 : listAdd (1 : l1) l1
       -- name a new subexpression
     == 1 : 1 : 2 : l2
        where l2 = listAdd (1 : l1) l1
                    -- unfold definition of l1
                 == listAdd (1 : l1) (2 : listAdd (1 : l1) l1)
                    -- replace `listAdd` with its definition
                 == 1 + 2 : listAdd l1 (listAdd (1 : l1) l1)
                    -- add numbers, replace subterm equivalent to l2
                 == 3 : listAdd l1 l2
       -- replace l2 with simpler version
     == 1 : 1 : 2 : 3 : listAdd l1 l2
       -- name a subexpression, etc
     == 1 : 1 : 2 : 3 : l3
        where l3 = listAdd l1 l2
                 == listAdd (2 : listAdd (1 : l1) l1) (3 : listAdd l1 l2)
                 == 2 + 3 : listAdd (listAdd (1 : l1) l1) (listAdd l1 l2)
                 == 5 : l2 + l3
     == 1 : 1 : 2 : 3 : 5 : l2 + l3
```

In the equations above, we are only just replacing subterms by equal
subterms. However, if we do this carefully, we can see what the values
of the list will be, if we ever need them for some computation.

More generally, because of laziness, our code is more modular. We have separated the
generation of the sequence of numbers from deciding just how
many of those numbers that we need. We don't compute all of the numbers
when we define this series --- we only compute them as we need them. This is
a powerful idea which will re-occur throughout the semester.

----------------------------------------------
For Penn students: There is a quiz associated with this module on Canvas. Please complete this quiz before the next class.

-}
