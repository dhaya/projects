> {-# LANGUAGE GADTs #-}
> module Oper where
>

Stack operations are push and pop



> data StackInstruction a where
>   Pop :: StackInstruction Int
>   Push :: Int -> StackInstruction ()
