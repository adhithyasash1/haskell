-- Importing the Stack module
import Stack

-- Defining a token type to represent values and operators
data Token = Val Int | Op Char
type Expr = [Token]

-- Evaluation Step:
-- Applies the operator on the top two elements of the stack and pushes the result back
evalStep :: Stack Int -> Token -> Stack Int
evalStep st (Val i) = push i st
evalStep st (Op c)
    | c == '+' = push (v2 + v1) st2
    | c == '-' = push (v2 - v1) st2
    | c == '*' = push (v2 * v1) st2
  where
    (v1, st1) = pop st
    (v2, st2) = pop st1

-- Evaluating an expression
evalExp :: Stack Int -> Expr -> Stack Int
evalExp st [] = fst (pop st)  -- When the expression is empty, return the result from the stack
evalExp st (t:ts) = evalExp (evalStep st t) ts


{- Here's what the code does:

-> It uses the Stack module that you've created earlier.
-> It defines a data type Token to represent values (Val Int) and operators (Op Char).
-> The evalStep function takes a stack and a token and performs the necessary evaluation 
   steps based on whether the token is a value or an operator.
-> The evalExp function evaluates a list of tokens representing a postfix expression using the evalStep function. 
   It repeatedly calls evalStep for each token in the expression until the entire expression is evaluated.

This implementation effectively demonstrates how to evaluate postfix expressions using a stack data structure and a step-by-step approach.-}