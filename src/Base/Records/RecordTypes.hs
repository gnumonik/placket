module RecordTypes where

import qualified Data.Text as T

type TypeString = T.Text

type ProtocolType = T.Text 

data Comp = EQ' | NOTEQ' | LT' | LTE' | GT' | GTE' deriving (Show, Eq)

data Operation = Plus Word | Minus Word deriving (Show, Eq)


--ProtocolBuilder Stuff
-------

-- Protocol Builder. No wildcards. Supports ranges + noncontigs. Only used for updates.
-- Wildcard means "default"
-- syntax: ARP [op=0-10 , sha=00:00:00:00:00:00 ]
data ProtocolBuilder = ProtocolBuilder TypeString FieldBuilderExp deriving (Show, Eq)

data FieldBuilder = FieldBuilder OpticStrs Field deriving (Show, Eq)-- need to pry apart 'field' for global vars 

data FieldBuilderExp = AllDefaults | FieldBuilderExp [FieldBuilder] deriving (Show, Eq)

data Field = SingleValue T.Text | RangeOfVals T.Text T.Text | NonContigSet [T.Text] deriving (Show, Eq)

type OpticStrs = [T.Text]



-- Predicate Data Structure & related functions 
------
data Predicate' a = ATOM a
                 | NOT (Predicate' a)
                 | (Predicate' a) :&&: (Predicate' a)
                 | (Predicate' a) :||: (Predicate' a) deriving (Show, Eq)

instance Functor Predicate' where
    fmap f (ATOM a) = ATOM (f a)
    fmap f (NOT as) = NOT (fmap f as)
    fmap f (as :&&: bs) = fmap f as :&&: fmap f bs
    fmap f (as :||: bs) = fmap f as :||: fmap f bs

instance Foldable Predicate' where
    foldr f e xs = let xs' = terms xs
                   in foldr f e xs'

instance Traversable Predicate' where
    traverse f ta = case ta of
        (ATOM a)     -> ATOM <$> f a
        (NOT as)     -> NOT  <$> traverse f as
        (as :&&: bs) -> pure (:&&:) <*> traverse f as <*> traverse f bs
        (as :||: bs) -> pure (:||:) <*> traverse f as <*> traverse f bs


evalPredicateM :: Monad m => Predicate' (m Bool) -> m Bool
evalPredicateM myPredicate = case myPredicate of
    (ATOM a)     -> a
    (NOT as)     -> not <$> evalPredicateM as
    (as :&&: bs) -> pure (\x y -> x &&  y) <*> (evalPredicateM as) <*> (evalPredicateM bs)
    (as :||: bs) -> pure (\x y -> x ||  y) <*> (evalPredicateM as) <*> (evalPredicateM bs)

evalPredicate :: Predicate' (Bool) -> Bool
evalPredicate myPredicate = case myPredicate of
    (ATOM a)     -> a
    (NOT as)     -> not $ evalPredicate as
    (as :&&: bs) ->  evalPredicate as &&  evalPredicate bs 
    (as :||: bs) -> evalPredicate as || evalPredicate bs 


terms :: Predicate' a -> [a]
terms (ATOM a)     = [a]
terms (NOT as)     = terms as
terms (as :&&: bs) = terms as <> terms bs
terms (as :||: bs) = terms as <> terms bs 



-- Selector Stuff
------

data MsgSelectorExp = MsgSelectorExp [MsgSelector] deriving (Show, Eq)

data MsgSelector = MsgSelector ProtocolSelector | MsgSelectorWC deriving (Show, Eq)

data ProtocolSelectorExp = ProtocolSelectorExp (Predicate' ProtocolSelector) deriving (Show, Eq)

data ProtocolSelector = ProtocolSelector TypeString FieldSelectorExp deriving (Show, Eq)

data FieldSelectorExp = FieldSelectorExp (Predicate' FieldSelector) | FieldSelectorExpWC deriving (Show, Eq)

data FieldSelector = FieldSelector OpticStrs Comp CompareTo deriving (Show, Eq)

data CompareTo = Literal Field | RefVarExpr OpticStrs (Maybe Operation) | CompareToWC deriving (Show, Eq)