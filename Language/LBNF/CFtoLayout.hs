{-#LANGUAGE TemplateHaskell#-}

{-
    BNF Converter: Layout handling Generator
    Copyright (C) 2004  Author:  Aarne Ranta
    Copyright (C) 2005  Bjorn Bringert

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}







module Language.LBNF.CFtoLayout(cf2Layout) where

import Data.List (sort)
import Data.Maybe (isNothing, fromJust)
import Language.LBNF.CF hiding (rename)

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

-- TODO: avoid using SYB
import Data.Generics as SYB

-- Generic renaming function
rename :: SYB.Data a => [(Name,Name)] -> a -> a
rename table = everywhere (mkT step)
  where
    step :: Name -> Name
    step x = maybe x id (lookup x table)

-- Dummy data types and functions
data Token = 
  Err Posn |
  PT Posn Tok 
data Tok =  TS !String Int
data Posn = Pn Int Int Int

data Block = Implicit Int
           | Explicit

layoutOpen'  = "{"
layoutClose' = "}"
layoutSep'   = ";"

cf2Layout :: CF -> Q [Dec]
cf2Layout cf = 
 let 
   (top,lay,stop) = layoutPragmas cf 
   mkRename = rename $ zip
      (id         [''Token, 'Err,  ''Posn, 'PT,  ''Tok, 'TS,  'Pn, ''Block, 'Implicit, 'Explicit])
      (map mkName ["Token", "Err", "Posn", "PT", "Tok", "TS", "Pn", "Block", "Implicit","Explicit"])
 in fmap mkRename $  
     fmap (DataD [] (mkName "Block") [] Nothing [NormalC (mkName "Implicit") [(Bang NoSourceUnpackedness NoSourceStrictness ,ConT ''Int)],
       NormalC (mkName "Explicit") []]
       ([DerivClause Nothing [ConT ''Show]])  :)
       (makedecs top lay stop (sort (reservedWords cf ++ symbols cf)))
       
-- Hack to make haddock work
makedecs :: Bool -> [String] -> [String] -> [String] -> Q [Dec]
makedecs top lay stop resws = [d|



  topLayout = $(lift top)
  layoutWords = $(lift lay)
  layoutStopWords = $(lift stop)

  layoutOpen  = $(lift layoutOpen')
  layoutClose = $(lift layoutClose')
  layoutSep   = $(lift layoutSep')
  

  resolveLayout tp = res Nothing [if tl then Implicit 1 else Explicit]
    where
    tl = tp && topLayout
    res _ [] ts = error $ "Layout error: stack empty. Tokens: " ++ show ts
  
    res _ st (t0:ts)
      | isLayoutOpen t0 = moveAlong (Explicit:st) [t0] ts
  
    res _ st (t0:ts)
      | isLayout t0 =
          case ts of
              t1:_ | isLayoutOpen t1 -> moveAlong st [t0] ts
              _ -> let col = if null ts then column t0 else column (head ts)
                       b:ts' = addToken (nextPos t0) layoutOpen ts
                       st' = Implicit col:st 
                    in moveAlong st' [t0,b] ts'
  
      | isLayoutClose t0 = 
            let st' = drop 1 (dropWhile isImplicit st)
             in if null st' 
                   then error $ "Layout error: Found " ++ layoutClose ++ " at (" 
                                  ++ show (line t0) ++ "," ++ show (column t0) 
                                  ++ ") without an explicit layout block."
                   else moveAlong st' [t0] ts
  
    res pt st@(Implicit n:ns) (t0:ts)
  
      | isStop t0 = 
         let (ebs,ns') = span (`moreIndent` column t0) ns
             moreIndent (Implicit x) y = x > y
             moreIndent Explicit _ = False
             b = 1 + length ebs
             bs = replicate b layoutClose
             (ts1,ts2) = splitAt (1+b) $ addTokens (afterPrev pt) bs (t0:ts)
          in moveAlong ns' ts1 ts2
  
      | newLine && column t0 < n  = 
         let b:t0':ts' = addToken (afterPrev pt) layoutClose (t0:ts)
          in moveAlong ns [b] (t0':ts')
  
      | newLine && column t0 == n = 
         if isNothing pt || isTokenIn [layoutSep,layoutOpen] (fromJust pt) 
            then moveAlong st [t0] ts
            else let b:t0':ts' = addToken (afterPrev pt) layoutSep (t0:ts)
                  in moveAlong st [b,t0'] ts'
     where newLine = case pt of
                             Nothing -> True
                             Just t  -> line t /= line t0
  
    res _ st (t:ts)  = moveAlong st [t] ts
  
    res (Just t) (Explicit:bs) [] | null bs = []
                                  | otherwise = res (Just t) bs []
  
    res (Just t) [Implicit n] []
        | isTokenIn [layoutSep] t = []
        | otherwise = addToken (nextPos t) layoutSep []
  
    res (Just t) (Implicit n:bs) [] =
       let c = addToken (nextPos t) layoutClose []
        in moveAlong bs c []
  
    res Nothing st [] = []
  
    moveAlong st [] ts = error $ "Layout error: moveAlong got [] as old tokens"
    moveAlong st ot ts = ot ++ res (Just $ last ot) st ts

  type Position = Posn

  isImplicit :: Block -> Bool
  isImplicit (Implicit _) = True
  isImplicit _ = False
   
  addTokens :: Position
            -> [String]
            -> [Token] 
            -> [Token]                       
  addTokens p ss ts = foldr (addToken p) ts ss

  addToken :: Position 
           -> String  
           -> [Token]
                       
         -> [Token]
  addToken p s ts = sToken p s : map (incrGlobal p (length s)) ts

  afterPrev = maybe (Pn 0 1 1) nextPos
  
  nextPos t = Pn (g + s) l (c + s + 1) 
    where Pn g l c = position t
          s = tokenLength t
  
  incrGlobal (Pn _ l0 _) i (PT (Pn g l c) t) =
    if l /= l0 then PT (Pn (g + i) l c) t
               else PT (Pn (g + i) l (c + i)) t
  incrGlobal _ _ p = error $ "cannot add token at " ++ show p
  
    
  reservedwords = $(resnum)
  
  sToken :: Position -> String -> Token
  sToken p s = PT p (TS s i)
    where i = maybe (error $ "not a reserved word: " ++ show s) id (lookup s reservedwords)

  position t = case t of
    PT p _ -> p
    Err p -> p
  
  line t = case position t of Pn _ l _ -> l
  
  column t = case position t of Pn _ _ c -> c
  
  isTokenIn ts t = case t of
    PT _ (TS r _) | elem r ts -> True
    _ -> False

  isLayout = isTokenIn layoutWords
  
  isStop = isTokenIn layoutStopWords
  
  isLayoutOpen = isTokenIn [layoutOpen]
  
  isLayoutClose = isTokenIn [layoutClose]
  
  tokenLength t = length $ $(varE $ mkName "prToken") t

  |]
  where resnum = lift $ zip resws ([1..] :: [Int])
   
