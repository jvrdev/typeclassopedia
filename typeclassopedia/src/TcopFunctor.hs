module TcopFunctor where

newtype Choice a b = Choice (Either a b)

instance Functor (Choice e) where
  fmap f (Choice (Right x)) = Choice (Right $ f x)
  fmap f (Choice (Left x )) = Choice (Left x)

