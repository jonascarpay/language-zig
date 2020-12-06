module VM where

data Stack

data Heap

data Frame = Frame
  { names
  , 

data VM = VM
  { stack :: Stack
  , stackPtr
  , heap :: Heap
  }

