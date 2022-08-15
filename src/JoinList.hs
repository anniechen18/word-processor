module JoinList where

import Sized

data JoinList m a = Empty
  | Single m a
  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)


tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

-- TODO: use infix notation to make it more readable
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty Empty = Empty
(+++) Empty a = a
(+++) a Empty = a
(+++) (Single m a) (Single n b) = Append (mappend m n) (Single m a) (Single n b)
(+++) (Append m (Single a b) (Single x y)) (Single n c) = Append (mappend m n) (Append m (Single a b) (Single x y)) (Single n c)
(+++) (Single n c) (Append m (Single a b) (Single x y)) = Append (mappend n m) (Single n c) (Append m (Single a b) (Single x y))
(+++) (Append m (Single a b) (Single c d)) (Append n (Single e f) (Single g h)) = Append (mappend m n) (Append m (Single a b) (Single c d)) (Append n (Single e f) (Single g h))
(+++) (Append m a b) (Append n c d) = Append (mappend m n) (Append m a b) (Append n c d)

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a

-- Retreives the element at index i, O(log(n)) time with size cached
indexJ _ Empty = Nothing
indexJ i _ | i < 0 = Nothing
indexJ i (Single m a) 
  | i > 0 = Nothing
  | i == 0 = Just a
indexJ i (Append m jl1 jl2)
  | i < s1 = indexJ i jl1
  | i > s1 = indexJ (i - s1) jl2
  where s1 = getSize . size $ tag jl1

-- Drops the first n elements from a join list
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ n jl | n <= 0 = jl
dropJ n (Single _ _) = Empty
dropJ n (Append m jl1 jl2) 
  | n == s1 = jl2
  | n < s1 = (dropJ n jl1) +++ jl2
  | otherwise = dropJ (n - s1) jl2
  where s1 = getSize . size $ tag jl1

-- Take first n elements form a join list
takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ n jl | n <= 0 = jl
takeJ n (Append m jl1 jl2)
  | n == s1 = jl1
  | n < s1 = takeJ n jl1
  | otherwise = jl1 +++ takeJ (n - s1) jl2
  where s1 = getSize . size $ tag jl1






