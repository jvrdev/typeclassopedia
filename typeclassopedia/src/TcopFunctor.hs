module TcopFunctor where

newtype Choice a b = Choice (Either a b)
                   deriving Show

instance Functor (Choice e) where
  fmap f (Choice (Right x)) = Choice (Right $ f x)
  fmap f (Choice (Left x )) = Choice (Left x)

newtype Funktion a b = Funktion (a -> b)

instance Functor (Funktion a) where
  fmap f (Funktion g) = Funktion (f . g)

data Pair a = Pair a a
            deriving Show
                     
instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

newtype Tup2 a b = Tup2 (a, b)
                 deriving Show

instance Functor (Tup2 a) where
  fmap f (Tup2 (a, b)) = Tup2 (a, f b)

data ITree a = Leaf (Int -> a) 
             | Node [ITree a]

instance Functor ITree where
  fmap f (Leaf g) = Leaf $ f . g
  fmap f (Node xs) = Node $ fmap (fmap f) xs


                                                     
