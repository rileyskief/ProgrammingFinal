Final Project: Graphing Calculator
=====================

> {-# OPTIONS_GHC -Wall #-}
> {-# LANGUAGE GADTs #-}
>
> module ProjFinal.Calc where
>
> import           Parsing2
> import qualified Data.Map as M
> import Data.Complex

> type Env = M.Map String Double

> description :: String
> description = unlines
>   [ "Welcome to our graphing calculator!"
>    "Features this calculator supports: floating-point numbers, standard arithmetic operations",
>    ", and error messages. It will graph the expression as well!",
>    "Type an expression, :help, or :quit." ]

> helpMsg :: String
> helpMsg = unlines
>   [ "You can use integers or floating point values,"
>   , "negation, or standard arithmetic operators + - * / ^ ."
>   , "Constants: pi, e"
>   , "Functions: sin, cos, tan, sqrt, abs"
>   , "Complex numbers: e.g., 3 + 2i, i + 2, sqrt(2 + 3i)"
>   , "Examples:"
>   , "  2 + 3i"
>   , "  (2 + 3i) * (4 + 6i)"
>   , "  sqrt(2 + 3i)" ]


> lexer :: TokenParser u
> lexer = makeTokenParser emptyDef
>   { reservedNames = ["let", "in", "if", "then", "else", "pi", "e", "sin", "cos", "tan", "sqrt", "abs"] }

> data InterpError where                       -- set up different error types
>   UndefinedVar   :: String -> InterpError   
>   DivisionByZero :: InterpError

> data Arith where                             -- set up different expression types
>   Num   :: Either Integer Double -> Arith     
>   Bin   :: Op -> Arith -> Arith -> Arith      -- take operation and the two numbers to apply it to
>   NegA  :: Arith -> Arith                     -- negation
>   Const :: String -> Arith                   -- For constants like pi and e
>   Func  :: String -> Arith -> Arith          -- For functions like sin, cos, etc.
>   ComplexNum :: Complex Double -> Arith           -- For complex numbers
>   Var :: String -> Arith
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

> parseComplex :: Parser Arith
> parseComplex = try parseFullComplex <|> try parseImaginary <|> parseReal
>   where
>     parseFullComplex = do
>       realPart <- naturalOrFloat >>= return . either fromIntegral id
>       imagPart <- reservedOp "+" *> naturalOrFloat <* reserved "i" >>= return . either fromIntegral id
>       return $ ComplexNum (realPart :+ imagPart)
>     parseImaginary = do
>       imagPart <- option 1 (naturalOrFloat >>= return . either fromIntegral id)  -- Default to 1 if no number is provided
>       reserved "i"
>       return $ ComplexNum (0 :+ imagPart)
>     parseReal = do
>       realPart <- naturalOrFloat >>= return . either fromIntegral id
>       return $ ComplexNum (realPart :+ 0)

> parseVar :: Parser Arith
> parseVar = do
>   varName <- identifier lexer
>   if varName == "x"
>     then return (Var "x")
>     else fail ("Unknown variable: " ++ varName)

> parseArithAtom :: Parser Arith
> parseArithAtom = parseComplex <|> parseConst <|> parseFunc <|> (Num <$> naturalOrFloat) <|> parseNeg <|> parens exprParser <|> parseVar

> parseConst :: Parser Arith
> parseConst = Const <$> (reserved "pi" *> pure "pi" <|> reserved "e" *> pure "e")

> parseFunc :: Parser Arith                                                                                   -- go through each until one succeeds
> parseFunc = do                                                                                              
>   funcName <- choice (map (\name -> reserved name >> return name) ["sin", "cos", "tan", "sqrt", "abs"])     -- >> return name to get string so that it works 
>   arg <- parens exprParser
>   return $ Func funcName arg

> parseNeg :: Parser Arith                        
> parseNeg = NegA <$> (reservedOp "-" *> (Num <$> naturalOrFloat))            -- parse negation
>

> eval :: String -> Either InterpError (Complex Double)        
> eval s = case parse arith s of
>   Left err   -> Left (UndefinedVar (show err))  -- Show error message
>   Right expr -> evalExpr M.empty expr           -- Evaluate the expression


> evalExpr :: Env -> Arith -> Either InterpError (Complex Double)
> evalExpr _ (Const "pi") = Right (pi :+ 0)
> evalExpr _ (Const "e") = Right (exp 1 :+ 0)
> evalExpr e (Func "sin" a) = fmap sin <$> evalExpr e a
> evalExpr e (Func "cos" a) = fmap cos <$> evalExpr e a
> evalExpr e (Func "tan" a) = fmap tan <$> evalExpr e a
> evalExpr e (Func "sqrt" a) = fmap sqrt <$> evalExpr e a
> evalExpr e (Func "abs" a) = do
>   result <- evalExpr e a
>   return (magnitude result :+ 0)  -- Wrap the magnitude result in the expected output of Complex Double
> evalExpr e (NegA a) = fmap negate <$> evalExpr e a
> evalExpr _ (Num i) = case i of
>   Left n  -> Right (fromIntegral n :+ 0)
>   Right n -> Right (n :+ 0)
> evalExpr e (Var v) =
>  case M.lookup v e of
>    Just val -> Right (val :+ 0)
>    Nothing  -> Left (UndefinedVar v)
> evalExpr _ (ComplexNum c) = Right c
> evalExpr e (Bin Add e1 e2) = (+) <$> evalExpr e e1 <*> evalExpr e e2
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
>         Left _     -> "Error: This should never happen"               -- this should already be handled, very bad if this happens
>         Right result -> prettyPrint result ++ "\n  = " ++ prettyPrint (ComplexNum expr)  -- Use prettyPrint for both input and result

> showInterpError :: InterpError -> String                                  -- explains errors to user
> showInterpError (UndefinedVar var) = "Undefined variable: " ++ var     
> showInterpError DivisionByZero     = "Division by zero"

> exprParser :: Parser Arith
> exprParser = buildExpressionParser table parseArithAtom         
>  where
>   table = [[Infix (Bin Exp <$ reservedOp "^") AssocRight ]           -- exponents are right associative
>           , [Infix (Bin Mul <$ reservedOp "*") AssocLeft ]            -- multiplication and division are left associative
>           , [Infix (Bin Div <$ reservedOp "/") AssocLeft ]            
>           , [Infix (Bin Add <$ reservedOp "+") AssocLeft ]            -- addition and subtraction are left associative
>           , [Infix (Bin Sub <$ reservedOp "-") AssocLeft ]]
>

> prettyPrint :: Arith -> String
> prettyPrint (Num n) = case n of
>   Left intVal  -> show intVal
>   Right dblVal -> show dblVal
> prettyPrint (Const "pi") = "π"
> prettyPrint (Const "e") = "e"
> prettyPrint (Func name arg) = name ++ "(" ++ prettyPrint arg ++ ")"
> prettyPrint (Bin Add e1 e2) = prettyPrint e1 ++ " + " ++ prettyPrint e2
> prettyPrint (Bin Sub e1 e2) = prettyPrint e1 ++ " - " ++ prettyPrint e2
> prettyPrint (Bin Mul e1 e2) = prettyPrint e1 ++ " * " ++ prettyPrint e2
> prettyPrint (Bin Div e1 e2) = prettyPrint e1 ++ " / " ++ prettyPrint e2
> prettyPrint (Bin Exp e1 e2) = prettyPrint e1 ++ " ^ " ++ prettyPrint e2
> prettyPrint (NegA e) = "-" ++ prettyPrint e
> prettyPrint (ComplexNum (r :+ i))
>   | r == 0 && i == 1  = "i"
>   | r == 0 && i == -1 = "-i"
>   | r == 0            = show i ++ "i"
>   | i == 0            = show r
>   | i > 0             = show r ++ " + " ++ show i ++ "i"
>   | otherwise         = show r ++ " - " ++ show (abs i) ++ "i" 

> buildFunction :: String -> Either String (Double -> Complex Double)
> buildFunction input = case parse arith input of
>   Left err -> Left (show err)
>   Right expr -> Right (\x -> case evalExpr (M.fromList [("x", x)]) expr of
>                                Right val -> val
>                                Left _ -> 0 :+ 0)  -- fallback for bad points

> graphExpr :: String -> String
> graphExpr input = case buildFunction input of
>   Left err -> "Error: " ++ err
>   Right f  -> renderGraph (realPart . f)  -- Graph only the real part

>
> calcu :: String -> String
> calcu input
>   | ":graph " `isPrefixOf` input = graphExpr (drop 7 input)
>   | otherwise =
>       case eval input of
>         Left err   -> showInterpError err
>         Right expr ->
>           case parse arith input of
>             Left _      -> "Error: This should never happen"
>             Right result -> prettyPrint result ++ "\n  = " ++ prettyPrint (ComplexNum expr)
> 
>