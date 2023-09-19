-- Custom Implementations of built in functions of Type class


-- When you use show on a Complex value, it will display it in a more human-readable format, handling both positive and negative imaginary parts.

data Complex = Complex Double Double

instance Show Complex where
    show (Complex real imag)
        | imag < 0 = show real ++ " - " ++ show (abs imag) ++ "i"
        | imag == 0 = show real
        | otherwise = show real ++ " + " ++ show imag ++ "i"


-- A custom ordering for a data type representing days of the week, where weekends come before weekdays

data Day = Sun | Mon | Tue | Wed | Thu | Fri | Sat
    deriving (Eq, Show)

instance Ord Day where
    compare Sun _ = LT
    compare _ Sun = GT
    compare _ _ = EQ


-- fmap applies the given function only if the value is wrapped in Some. If it's None, it remains unchanged.

data Optional a = Some a | None
    deriving Show

instance Functor Optional where
    fmap _ None = None
    fmap f (Some x) = Some (f x)


-- The Monoid type class is used for types that can be combined and have an identity element. 
-- Let's create a custom Monoid instance for a type representing concatenation of strings

newtype StringConcat = StringConcat String

instance Monoid StringConcat where
    mempty = StringConcat ""
    mappend (StringConcat s1) (StringConcat s2) = StringConcat (s1 ++ s2)


-- You can customize arithmetic operations for a data type by implementing the Num type class. 
-- Let's create a custom Num instance for a simple polynomial data type. 
-- This Num instance allows you to add and subtract polynomials element-wise, and convert integers to polynomial constants.

data Polynomial = Poly [Double]

instance Num Polynomial where
    (Poly p1) + (Poly p2) = Poly (zipWith (+) p1 p2)
    (Poly p1) - (Poly p2) = Poly (zipWith (-) p1 p2)
    _ * _ = error "Multiplication is not supported for polynomials."
    abs (Poly p) = Poly (map abs p)
    signum _ = error "Signum is not supported for polynomials."
    fromInteger x = Poly [fromIntegral x]
