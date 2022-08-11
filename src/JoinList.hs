module JoinList where

data JoinList m a = Empty
  | Single m a
  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)


tag :: Monoid m => JoinList m a -> m
--mempty returns unit. how do we invoke the mempty of m? i.e. should return product 1 if m is product
-- Empty nodes do not explicitly store an annotation, but we consider them to have an annotation of mempty (that is, the identity element for the given monoid).
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m


(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty Empty = Empty
(+++) Empty a = a
(+++) a Empty = a
(+++) (Single m a) (Single n b) = Append (mappend m n) (Single m a) (Single n b)
(+++) (Append m (Single a b) (Single x y)) (Single n c) = Append (mappend m n) (Append m (Single a b) (Single x y)) (Single n c)
(+++) (Single n c) (Append m (Single a b) (Single x y)) = Append (mappend n m) (Single n c) (Append m (Single a b) (Single x y))
(+++) (Append m (Single a b) (Single c d)) (Append n (Single e f) (Single g h)) = Append (mappend m n) (Append m (Single a b) (Single c d)) (Append n (Single e f) (Single g h))
(+++) (Append m a b) (Append n c d) = Append (mappend m n) (Append m a b) (Append n c d)

