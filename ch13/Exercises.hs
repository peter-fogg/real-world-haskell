import Data.List

data Op = Plus | Minus | Mul | Div | Pow
        deriving (Eq)

data SymbolicManip a = 
          Number a           -- Simple number, such as 5
        | Symbol String      -- A symbol, such as x
        | BinaryArith Op (SymbolicManip a) (SymbolicManip a)
        | UnaryArith String (SymbolicManip a)
          deriving (Eq)

instance Num a => Num (SymbolicManip a) where
    a + b = BinaryArith Plus a b
    a - b = BinaryArith Minus a b
    a * b = BinaryArith Mul a b
    negate a = BinaryArith Mul (Number (-1)) a
    abs a = UnaryArith "abs" a
    signum _ = error "signum is unimplemented"
    fromInteger i = Number (fromInteger i)

instance (Fractional a) => Fractional (SymbolicManip a) where
    a / b = BinaryArith Div a b
    recip a = BinaryArith Div (Number 1) a
    fromRational r = Number (fromRational r)

instance (Floating a) => Floating (SymbolicManip a) where
    pi = Symbol "pi"
    exp a = UnaryArith "exp" a
    log a = UnaryArith "log" a
    sqrt a = UnaryArith "sqrt" a
    a ** b = BinaryArith Pow a b
    sin a = UnaryArith "sin" a
    cos a = UnaryArith "cos" a
    tan a = UnaryArith "tan" a
    asin a = UnaryArith "asin" a
    acos a = UnaryArith "acos" a
    atan a = UnaryArith "atan" a
    sinh a = UnaryArith "sinh" a
    cosh a = UnaryArith "cosh" a
    tanh a = UnaryArith "tanh" a
    asinh a = UnaryArith "asinh" a
    acosh a = UnaryArith "acosh" a
    atanh a = UnaryArith "atanh" a

prettyShow :: (Show a, Num a) => SymbolicManip a -> String
prettyShow (Number x) = show x
prettyShow (Symbol x) = x
prettyShow (BinaryArith op a b) =
    let pa = simpleParen a
        pb = simpleParen b
        pop = op2str op
        in pa ++ pop ++ pb
prettyShow (UnaryArith opstr a) = 
    opstr ++ "(" ++ show a ++ ")"

-- This code is non-awesome, but it works!?
paren (BinaryArith op left right) =
  if op `elem` [Plus, Minus]
  then "(" ++ prettyShow' left ++ show op ++ prettyShow' right ++ ")"
  else prettyShow' left ++ show op ++ prettyShow' right
paren exp = prettyShow' exp

prettyShow' :: (Show a, Num a) => SymbolicManip a -> String
prettyShow' (Number x) = show x
prettyShow' (Symbol x) = x
prettyShow' (BinaryArith Plus a b) = prettyShow' a ++ "+" ++ prettyShow' b
prettyShow' (BinaryArith Minus a b) = prettyShow' a ++ "-" ++ prettyShow' b
prettyShow' (BinaryArith op a b) = paren a ++ show op ++ paren b
prettyShow' (UnaryArith opstr a) = opstr ++ " " ++ paren' a
  where paren' (Number x) = show x
        paren' (Symbol x) = x
        paren' exp = "(" ++ prettyShow' exp ++ ")"

op2str :: Op -> String
op2str Plus = "+"
op2str Minus = "-"
op2str Mul = "*"
op2str Div = "/"
op2str Pow = "**"

simpleParen :: (Show a, Num a) => SymbolicManip a -> String
simpleParen (Number x) = prettyShow (Number x)
simpleParen (Symbol x) = prettyShow (Symbol x)
simpleParen x@(BinaryArith _ _ _) = "(" ++ prettyShow x ++ ")"
simpleParen x@(UnaryArith _ _) = prettyShow x

instance (Show a, Num a) => Show (SymbolicManip a) where
    show a = prettyShow' a

instance Show Op where
  show = op2str
