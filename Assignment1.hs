-- Author:   Jing Du <du2@student.unimelb.edu.au>
-- Purpose:  COMP90048 Declarative Programming Assignment1 

module Assignment1(subst, interleave, unroll) where
   -- function1 subst :: Eq t => t -> t -> [t] -> [t]
   subst:: Eq t => t -> t -> [t] -> [t]
   subst old new [] = []
   subst old new (x:xs)
      |x == old  = new:(subst old new xs)
      |otherwise = x:(subst old new xs)
   
   -- function2 interleave :: [t] -> [t] -> [t]
   interleave:: [t] -> [t] -> [t]
   interleave (x:xs) ys = x : interleave ys xs
   interleave [] ys     = ys
   
   -- function3 unroll :: Int -> [a] -> [a]
   unroll :: Int -> [t] -> [t]
   unroll a [] = []
   unroll a t
      |a > (length t)  = t ++ (unroll(a-length(t))(t))
      |otherwise       = take a t


