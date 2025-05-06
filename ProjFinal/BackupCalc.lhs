
Project 2: Calculator
=====================

For this project, you will implement the guts of a (fancy) calculator.
I have provided you with a simple read-eval-print interface (in
[CalcREPL.hs](CalcREPL.hs)) that lets the user type in expressions to
be evaluated.  You will ultimately provide a function of type `String
-> String` which accepts the user's input and produces a response.  Of
course, your `String -> String` function should be decomposed into
multiple phases, just like all of the language implementations we have
been considering (such as parsing, pretty-printing, interpreting, and
so on).  This project intentionally gives you much less guidance than
the first project in terms of what specific data types and functions
you should write, and how to decompose your solution.  However, you
can of course use the modules we have done as templates to help guide
you.

Getting started
---------------

- Download the [provided zip file](calc.zip), which should contain several
  files including [`CalcREPL.hs`](CalcREPL.hs),
  [`Calc.lhs`](Calc.lhs), [`Parsing2.hs`](Parsing2.hs), and a
  few configuration files such as `calc.cabal` and `stack.yaml`.

- Extract the contents of the zip file.

- While working on your calculator, to load it into `ghci` (e.g. in
   order to try out a function), you can use the `stack repl` command.

- To compile and run your calculator, you can use the command `stack run`
  (this should be typed at a terminal/shell prompt, not at a ghci
  prompt).

    - You should get a calculator prompt where you can enter expressions (though
      it will not do anything yet).

    - Simply exit the calculator and rerun `stack run` every time
      you want to test changes you have made to `Calc.lhs`.

Level 1
-------

Your calculator must support the following features:

- Floating-point numbers (represented as `Double` values)
- Standard arithmetic operations `+`, `-`, `*`, `/`, and `^`
  as well as prefix negation
- Display appropriate error messages to the user instead of crashing
- Display appropriate startup and `:help` messages which
  explain/illustrate the features of the calculator.

For example, a sample interaction with your calculator might look like
this:

    > 2+3
    5
    > (((3*5)   -   9)  + -8.3)
    -2.3000000000000007
    > 2 ^ 2 ^ 2 ^ 2
    65536.0
    > (3+3)*3
    18.0
    > 3+
    (line 1, column 3):
    unexpected end of input
    expecting end of "+", number, or "("

Your calculator must never crash with a runtime error or
pattern-match failure.

Get started by editing the starter code below and adding to it as
appropriate!

General notes and hints
-----------------------

+ You can use the `reserved` token parser to parse things like
  function names, names of constants or units, *etc.*
+ You can use the `naturalOrFloat` token parser to parse literal
  values that can either be an integer or a floating-point value. Note
  it does not handle negatives; that should be taken care of
  automatically by your prefix negation operator.
+ You can use `fromIntegral` to convert from `Integer` to `Double`.
+ You should use the `parse` function to run your parser.  If it
  returns an error wrapped in a `Left` constructor, you can simply
  call `show` on the resulting error to turn it into a `String`
  appropriate for displaying to the calculator user.
+ The `parseSome` function can be used as before for experimenting
  with parsers in GHCi.
+ Exponentiation for `Double` values in Haskell is done with the
  `(**)` operator.  (The `(^)` operator is only for integers.)

Starter code
------------

> {-# OPTIONS_GHC -Wall #-}
> {-# LANGUAGE GADTs #-}
>
> module Calc where
>
> import           Parsing2
> import qualified Data.Map as M


  

> type Env = M.Map String Integer

Edit this description and replace it with your own!  It gets printed
when the calculator interface first starts up.

> description :: String
> description = unlines
>   [ "Welcome to my calculator."
>   , "Programming Languages Project 2 - 2025"
>   , "Mason Dougan"
>   , "Features this calculator supports: floating-point numbers, standard arithmetic operations"
>   , ", and error messages."
>   , "Type an expression, :help, or :quit."
>   ]

Edit this help message and replace it with your own! It gets printed
when the user types `:help`.  Adding some well-chosen examples could
be a good way to concisely show off the different features of your
calculator.

> helpMsg :: String
> helpMsg = unlines
>   [ "You can use integers or floating point values,"
>   , "negation, or standard arithmetic operators + - * / ^ ."
>   ]

This is the main function that is called by `CalcREPL` to evaluate
user input.


> lexer :: TokenParser u
> lexer = makeTokenParser emptyDef
>   { reservedNames = ["let", "in", "if", "then", "else"] }                  
>
>


> data InterpError where                       -- set up different error types
>   UndefinedVar   :: String -> InterpError   
>   DivisionByZero :: InterpError

> data Arith where                             -- set up different expression types
>   Num  :: Either Integer Double -> Arith     
>   Bin  :: Op -> Arith -> Arith -> Arith      -- take operation and the two numbers to apply it to
>   NegA :: Arith -> Arith                     -- negation
>   deriving (Show)

> data Op where                           
>   Add :: Op
>   Sub :: Op
>   Mul :: Op
>   Div :: Op
>   Exp :: Op
>   deriving (Show, Eq)


> whiteSpace :: Parser ()                     -- deals with whitespace
> whiteSpace = getWhiteSpace lexer              

> arith :: Parser Arith                      
> arith = whiteSpace *> exprParser <* eof          -- parse the expression and ignore whitespace

> parens :: Parser a -> Parser a                   
> parens = getParens lexer                     -- parse expressions in parentheses

> reservedOp :: String -> Parser ()              --
> reservedOp = getReservedOp lexer               -- parse reserved operators

> reserved :: String -> Parser ()                 
> reserved = getReserved lexer                  

> naturalOrFloat :: Parser (Either Integer Double)
> naturalOrFloat = getNaturalOrFloat lexer

> parseArithAtom :: Parser Arith                                               -- parse the expression to the data type it should be
> parseArithAtom = (Num <$> naturalOrFloat) <|> parseNeg <|> parens exprParser        

> parseNeg :: Parser Arith                        
> parseNeg = NegA <$> (reservedOp "-" *> (Num <$> naturalOrFloat))            -- parse negation
>

> eval :: String -> Either InterpError Double            -- function to evaluate the expression
> eval s = case parse arith s of
>   Left err   -> Left (UndefinedVar (show err))        -- show error message
>   Right expr -> evalExpr M.empty expr                 -- run expression through evalExpr


> evalExpr :: Env -> Arith -> Either InterpError Double                         
> evalExpr e (NegA a) = (-) <$> evalExpr e (Num (Right 0)) <*> evalExpr e a     -- handles negation    
> evalExpr _ (Num i) = case i of                                                -- check if number is integer or double
>   Left n  -> Right (fromIntegral n)                                           -- convert integer to double
>   Right n -> Right n                                                          -- return double value
> evalExpr e (Bin Add e1 e2) = (+) <$> evalExpr e e1 <*> evalExpr e e2          -- add two numbers etc. etc.
> evalExpr e (Bin Sub e1 e2) = (-) <$> evalExpr e e1 <*> evalExpr e e2
> evalExpr e (Bin Mul e1 e2) = (*) <$> evalExpr e e1 <*> evalExpr e e2
> evalExpr e (Bin Div e1 e2) = (/) <$> evalExpr e e1 <*> evalExpr e e2
> evalExpr e (Bin Exp e1 e2) = (**) <$> evalExpr e e1 <*> evalExpr e e2



> calc :: String -> String                  -- main function to take user input and return result
> calc input =
>   case eval input of
>     Left err   -> showInterpError err               -- show error message
>     Right expr ->                                   
>       case parse arith input of    
>         Left _     -> "Error: This should never happen"               -- this should already be handled, very bad if this happen
>         Right result -> show (prettyPrint result) ++ "\n  = " ++ show expr         -- run result of expression through pretty print 
>                                                                                     -- for readability


> showInterpError :: InterpError -> String                                  -- explains errors to user
> showInterpError (UndefinedVar var) = "Undefined variable: " ++ var     
> showInterpError DivisionByZero     = "Division by zero"



> exprParser :: Parser Arith
> exprParser = buildExpressionParser table parseArithAtom         
>  where
>   table = [ 
>             [Infix (Bin Exp <$ reservedOp "^") AssocRight ]           -- exponents are right associative
>           , [Infix (Bin Mul <$ reservedOp "*") AssocLeft ]            -- multiplication and division are left associative
>           , [Infix (Bin Div <$ reservedOp "/") AssocLeft ]            
>           , [Infix (Bin Add <$ reservedOp "+") AssocLeft ]            -- addition and subtraction are left associative
>           , [Infix (Bin Sub <$ reservedOp "-") AssocLeft ]
>           ]

 
> prettyPrint :: Arith -> String                                                    -- formats the result for output
> prettyPrint (Num n) = case n of
>   Left intVal  -> show intVal                                                     -- extract actual values from the Either type
>   Right dblVal -> show dblVal                                                     -- print the number
> prettyPrint (Bin Add e1 e2) = prettyPrint e1 ++ " + " ++ prettyPrint e2           -- print it with the operator for readability
> prettyPrint (Bin Sub e1 e2) = prettyPrint e1 ++ " - " ++ prettyPrint e2
> prettyPrint (Bin Mul e1 e2) = prettyPrint e1 ++ " * " ++ prettyPrint e2
> prettyPrint (Bin Div e1 e2) = prettyPrint e1 ++ " / " ++ prettyPrint e2
> prettyPrint (Bin Exp e1 e2) = prettyPrint e1 ++ " ^ " ++ prettyPrint e2
> prettyPrint (NegA e) = "-" ++ prettyPrint e


Level 2
-------

To complete this project to Level 2, in addition to the requirements
for Level 1:

- Re-display a nicely formatted version of the user's input as
  confirmation of each computation.  For example, a sample interaction
  with your calculator might now look like this:

    ```
    > 2+3
    2.0 + 3.0
      = 5.0
    > (((3*5)  -   9)  + -8.3)
    3.0 * 5.0 - 9.0 + -8.3
      = -2.3000000000000007
    > 2 ^ 2 ^ 2 ^ 2
    2.0 ^ 2.0 ^ 2.0 ^ 2.0
      = 65536.0
    > (3+3)*3
    (3.0 + 3.0) * 3.0
      = 18.0
    ```

- Ensure that your code uses [good Haskell style](https://kowainik.github.io/posts/2019-02-06-style-guide).

- Make sure your code is simplified as much as possible, for example,
  without redundant pattern-matching.

- Turn on `{-# OPTIONS_GHC -Wall #-}` and make sure your code generates no warnings.

- Write informative, grammatically correct comments explaining your
   code, its operation, and any choices you made along with the
   reasons for those choices.

Level 3
-------

To complete this project to Level 3, in addition to the requirements
for Level 2, you must complete *at least two* extensions.  You may
pick from the following list of suggested extensions (ordered roughly
from easier to harder), or propose your own.

1. Add support for the constants $\pi$ and $e$, along with at least
   five functions such as sine, cosine, tangent, log, floor, ceiling,
   round, square root, or absolute value.  For example, a sample
   interaction might look like this:

    ```
    > sin(pi/6)
    sin(π / 6.0)
      = 0.49999999999999994
    > cos(tan(log(abs(-2))))
    cos(tan(log(abs(-2.0))))
      = 0.6744026976311414
    > ((1 + sqrt(5))/2)^2 - 1
    ((1.0 + sqrt(5.0)) / 2.0) ^ 2.0 - 1.0
      = 1.618033988749895
    ```

2. Support for complex numbers.  For example, the user should be able
   to enter expressions like `3 + 2i`.  Note that real numbers should
   never be pretty-printed with an imaginary component, and purely
   imaginary numbers should not be pretty-printed with a real
   component.  For example,

    ```
    > 2
    2.0
      = 2.0
    > 3i
    3.0i
      = 3.0i
    > i + 2
    i + 2.0
      = 2.0 + i
    > 2 + 3i
    2.0 + 3.0i
      = 2.0 + 3.0i
    > (2 + 3i) * (4 + 6i)
    (2.0 + 3.0i) * (4.0 + 6.0i)
      = -10.0 + 24.0i
    > sqrt(2 + 3i)
    sqrt(2.0 + 3.0i)
      = 1.6741492280355401 + 0.8959774761298381i
    ```

    (The last example works only if you have also implemented the
    first extension.)

    You can import the `Complex.Double` module to work with complex
    numbers in Haskell.

    Note there is a slight wrinkle to deal with when parsing a literal
    imaginary value: if you see a number you do not yet know whether
    it will be followed by `i` or not.  The problem is that by
    default, if a parsec parser consumes some input before failing, it
    does *not* backtrack to try re-parsing the same input.  So, as an example,
    something like this:

    ```
    Imag <$> (integer <* reserved "i") <|> Real <$> integer
    ```

    does *not* work, since if there is an integer not followed by an
    `i`, the first parser will irreversibly consume the integer before
    failing to find an `i`; when the second parser is tried there will
    no longer be an integer for it to find.

    The solution is that any parser which you would like to backtrack
    can be wrapped in the `try` function.  So

    ```
    Imag <$> try (integer <* reserved "i") <|> Real <$> integer
    ```

    works as expected: if there is no `i` following an integer and the
    first parser fails, the input gets rewound to the beginning of the
    integer before trying the second parser.

3. Support for units of measurement.  Pick a domain (*e.g.* length,
   mass, time, ...) and allow the user to add units in that domain to their
   calculations.  For example  (yours does not have to work exactly
   like this):

    ```
    > 1
    1.0
      = 1.0
    > 1 inch
    1.0 in
      = 1.0 in
    > 1 inch + 3 inches
    1.0 in + 3.0 in
      = 4.0 in
    > 1 meter + 1 inch
    1.0 m + 1.0 in
      = 1.0254 m
    > (1 meter + 1 inch) as inches
    (1.0 m + 1.0 in) as in
      = 40.370078740157474 in
    > ((1.6 ft * 700 + 8.1 ft) / 2) as miles
    ((1.6 ft * 700.0 + 8.1 ft) / 2.0) as mi
      = 0.10678412422360248 mi
    > 5 feet * 2 meters
    5.0 ft * 2.0 m
      = Error: tried to multiply two values with units, namely 5.0 ft and 2.0 m
    > 5 km + 6
    5.0 km + 6.0
      = Error: tried to add values with and without units, namely 5.0 km and 6.0
    > (5 km) mi
    5.0 km mi
      = Error: tried to apply units mi to a value that already had units km
    > (5 km) as mi
    5.0 km as mi
      = 3.105590062111801 mi
    > 6 as cm
    6.0 as cm
      = Error: can't convert scalar 6.0 to cm
    ```

    Some hints:

    + It should be possible to add two values with units, with
      conversion as appropriate.  It should be an error to add a value
      with units to a value without units.
    + It should be possible to multiply a value with units by a value
      without units, or vice versa.  It should be an error to multiply
      two values with units.
    + It is an error to do exponentiation with anything other than
      unitless values.
    + You will need to change your interpreter quite a bit: it will
      need to keep track of which values have units attached and which
      do not.  It also now has the possibility of generating a runtime
      error.
    + In the example above, units can be introduced by adding a unit
      to a value as a suffix: this makes a unitless value into a value
      with a unit, or checks that a value with units has the indicated
      units.  Alternatively, a conversion can be indicated by writing
      "as <unit>"; this convets a value with units into the indicated
      units, and is an error for values without units.  See the above
      examples.  This is just a suggestion; you do not have to
      organize your calculator in exactly this way.

4. Support for simple algebraic expressions involving polynomials.
   For example:

    ```
    > (x+1)^2
    (x + 1)^2
      = x^2 + 2*x + 1
    > (x+1)*(y-3)
    (x + 1) * (y - 3)
      = x * y - 3 * x + y - 3
    > (x^2 + 3*x + 1) / (x + 1)
      Sorry, division of polynomials is not supported.
    ```

    If you want to be really fancy you could support polynomial
    division too:

    ```
    > (x^2 + 3*x + 1) / (x + 1)
    (x^2 + 3 * x + 1) / (x + 1)
      = x + 2 - 1 / (x + 1)
    ```

5. You should also feel free to propose your own extensions; just be
   sure to run them by me to make sure you choose something with an
   appropriate level of difficulty.
