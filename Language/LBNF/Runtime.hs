{-#LANGUAGE TemplateHaskell #-}
-- | Contains things that are typically needed in modules that use 
-- languages defined using BNFC-meta.
module Language.LBNF.Runtime(
  -- * Happy and Alex runtimes
  -- ord
  -- , listArray
  -- , (!)
  -- , Array
  -- , parseToQuoter
  
  ParseMonad(..)
  , err
  
  -- * Pretty printing runtimes
  , printTree
  , Doc
  , doc
  , concatD
  , Print(..)
  , prPrec
  , PrintPlain(..)

  ) where



import Control.Monad (MonadPlus(..), liftM, foldM, (>=>), ap)
import Control.Applicative ( Applicative(..) )


import Data.Char



------------------
-- Lexing, Parsing
------------------

-- * The result of a parse.
data ParseMonad a = Ok a | Bad String
  deriving (Read, Show, Eq, Ord)

instance Monad ParseMonad where
  return      = Ok
  fail        = Bad
  Ok a  >>= f = f a
  Bad s >>= f = Bad s

instance Functor ParseMonad where
  fmap = liftM

instance Applicative ParseMonad where
  (<*>) = ap
  pure = return

--instance MonadPlus ParseMonad where
--  mzero = Bad "Err.mzero"
--  mplus (Bad _) y = y
--  mplus x       _ = x

-- * An eliminator for a parse result. Takes a function that recovers from any 
-- parse errors. Typical usage: @err error (pCategory (tokens s)) :: Category@ 
err :: (String -> a) -> ParseMonad a -> a
err e b = case b of 
    Bad s -> e s
    Ok x  -> x


-----------
-- PRINTING
-----------

-- * Overloaded pretty-printer
printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : "," :ts -> showString t . space "," . rend i ts
    t  : ")" :ts -> showString t . rend i (")":ts)
    t  : "]" :ts -> showString t . rend i ("]":ts)
    t        :ts -> space t . rend i ts
    _            -> id
  new i   = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t = showString t . (\s -> if null s then "" else (' ':s))

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- the printer class does the job
class Print a where
  prt :: Int -> a -> Doc
  prtList :: [a] -> Doc
  prtList = concatD . map (prt 0)

instance Print a => Print [a] where
  prt _ = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j<i then parenth else id


instance Print Integer where
  prt _ x = doc (shows x)
  prtList es = case es of
   [] -> (concatD [])
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ",") , prt 0 xs])


instance Print Double where
  prt _ x = doc (shows x)

newtype PrintPlain = MkPrintPlain String

instance Print PrintPlain where
  prt _ (MkPrintPlain s) = doc $ showString s