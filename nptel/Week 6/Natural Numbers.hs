-- Define a recursive data type 'Nat' for natural numbers
data Nat = Zero | Succ Nat

-- 'Zero' represents 0, 'Succ' represents the successor of a number

-- Check if a 'Nat' is zero
iszero :: Nat -> Bool
iszero Zero = True
iszero (Succ _) = False

-- Get the predecessor of a 'Nat'
pred :: Nat -> Nat
pred Zero = Zero
pred (Succ n) = n

-- Addition of two 'Nat' numbers
plus :: Nat -> Nat -> Nat
plus m Zero = m
plus m (Succ n) = Succ (plus m n)

-- Multiplication of two 'Nat' numbers
mult :: Nat -> Nat -> Nat
mult m Zero = Zero
mult m (Succ n) = plus (mult m n) m

-- Example usage:
-- Creating 'Nat' numbers
let zero = Zero
let one = Succ Zero
let two = Succ (Succ Zero)

-- Checking if 'Nat' numbers are zero
iszero zero  -- True
iszero one   -- False

-- Finding the predecessor of a 'Nat' number
pred two  -- Succ Zero

-- Adding 'Nat' numbers
plus one two  -- Succ (Succ (Succ Zero))

-- Multiplying 'Nat' numbers
mult two two  -- Succ (Succ (Succ (Succ Zero)))
