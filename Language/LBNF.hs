{-# OPTIONS_GHC -fno-warn-missing-fields #-}

-- | Contains the main language definition routines of BNFC-meta.
module Language.LBNF (
  -- * Main language definition routines
    lbnf                 
  , bnfc
  -- * Debugging functions
  , dumpCode
  , getCode
  , dumpAlex 
  , dumpHappy
--  , dumpHappyM
  , module Language.LBNF.Compiletime
--  , module Language.LBNF.Grammar
  ) where

import Language.LBNF.CFtoAbstract(absRules,absTokens)
import Language.LBNF.CFtoAlex2(cf2alex2, abstractAlex, concreteAlex)
import Language.LBNF.CFtoHappy(cf2Happy, abstractHappy, concreteHappy)
import Language.LBNF.CFtoQQ(cf2qq)
import Language.LBNF.CFtoPrinter
import Language.LBNF.CFtoLayout

import Language.LBNF.Compiletime
import Language.LBNF.CF(CF, visibleNames, allCats, isNormal, hasIdent, hasLayout)
import Language.LBNF.Grammar

import Language.LBNF.GetCF

import Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Hide

import Data.List(isPrefixOf, intersperse)

-- | QuasiQuoter for LBNF. Usage: @[lbnf| \<grammar for your language\>|]@
-- 
-- The spliced code is an expression of type 'Grammar'.
lbnf :: QuasiQuoter
lbnf = grammar

-- | Typical usage (as a top level declaration): @bnfc [lbnf|\<grammar\>|]@
-- 
-- The spliced code contains a parser, a pretty-printer and a Quasi-Quoter for 
-- the language defined by the grammar.
bnfc :: Grammar -> Q [Dec]
bnfc = compile . toCF 

toCF :: Grammar -> CF
toCF g = case getCFofG (return g) of
    (cf,[]) -> cf
    (_,msgs) -> error $ unlines msgs

compile :: CF -> Q [Dec]
compile g = do 
  l <- location
--  m <- fmap loc_module location
  datas  <- absRules g
  tokens <- absTokens g
  pretty <- cf2Printer g
  dEp    <- cf2qq g
  dalx   <- abstractAlex g
  dhpy   <- abstractHappy l g
  dLayout <- if hasLayout g then cf2Layout g else return [] 
  hidden <- hide g $ concat 
        [ datas 
        , tokens
        , pretty
        , dEp
        , dalx 
        , dhpy
        , dLayout
        ]
  return $ concat 
        [ hidden
        ]


hide cf = export (map mkName $ visibleNames cf ++ if hasLayout cf then ["resolveLayout"] else []) where

exportList m cf = 
  "(" ++ concat (intersperse "\n  , " $ map (m++) ns) ++ ")" 
  where
    ns = visibleNames cf ++ map (++"(..)")
      (filter isNormal (allCats cf)) ++ if hasIdent cf then ["Ident(..)"] else []


-- | Equivalent to 'bnfc' with the side-effect of dumping the parser 
-- definition into a file named "dump.y"
dumpHappy :: Grammar -> Q [Dec]
dumpHappy g = location >>= \l -> dumpHappyM g (loc_module l, loc_package l) >> bnfc g

dumpHappyM :: Grammar -> (String,String) -> Q ()
dumpHappyM g (p,m) = do
  let cf = toCF g
  runIO $ writeFile "dump.y" $ cf2Happy (Loc {loc_module = m, loc_package = p}) cf
  -- compile cf

-- | Equivalent to 'bnfc' with the side-effect of dumping the lexer 
-- definition into a file named dump.x
dumpAlex :: Grammar -> Q [Dec]
dumpAlex g = do
  let cf = toCF g
  runIO $ writeFile "dump.x" $ cf2alex2 cf
  compile cf
  
-- | Equivalent to 'bnfc' with the side-effect of dumping ALL the spliced code 
-- into a file named dump.hs
dumpCode :: Grammar -> Q [Dec]
dumpCode g = do
  runIO $ getCode ("Main") g >>= writeFile "dump.hs"
  bnfc g

-- | Computes the Haskell source code that would be spliced by a grammar and 
-- wraps it into a compilable Haskell module.
getCode :: String -> Grammar -> IO String
getCode m g = do
  let cf = toCF g
  datas  <- runQ $ absRules cf
  tokens <- runQ $ absTokens cf
  pretty <- runQ $ cf2Printer cf
  dEp    <- runQ $ cf2qq cf
  let header = unlines
        [ "{-# LANGUAGE TemplateHaskell #-}"
        , "{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}"
        , "module "++m++exportList (m++".") cf++" where"
        , "import Language.LBNF.Compiletime"
        , "import Language.Haskell.TH(lift,loc_package,location)"
        ]
      res = unlines [
        header,
        uglyPrint $ pprint $ concat [datas , tokens, pretty, dEp],
        concreteAlex cf,
        concreteHappy (Loc {loc_module = m, loc_package = " $(fmap loc_package location >>= lift) "}) cf
        ]
  return res
            
  

uglyPrint = 
  subst "GHC.Base." "" . 
  subst "GHC.Show." "" .
  subst "Language.LBNF.ParseMonad." "" . 
  subst "Language.LBNF.Runtime." "" . 
  subst "GHC.List." "" .
  subst "GHC.Classes." "" .
  subst "GHC.Types." ""

test = "                                                                                                                                                                                'e'])])\n" ++ 
       "global_aq (AqToken a_10) = Language.LBNF.Runtime.aqFromString ((drop 2 GHC.Base.. (reverse GHC.Base.. (GHC.List.drop 3 GHC.Base.. GHC.List.reverse))) GHC.Base.$ Language.LBNF.Runtime.printTree a_10)"


subst _    _  []        = []
subst from to xs@(a:as) =
    if isPrefixOf from xs
        then to ++ subst from to (drop (length from) xs)
        else a : subst from to as



