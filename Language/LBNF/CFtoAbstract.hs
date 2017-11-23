{-# LANGUAGE TemplateHaskell #-}
{-
    BNF Converter: Abstract syntax Generator
    Copyright (C) 2004  Author:  Markus Forberg

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

module Language.LBNF.CFtoAbstract (absRules,absTokens) where

import Language.Haskell.TH

import Language.LBNF.CF



absRules :: CF -> Q [Dec]
absRules cf0 = sequence $ 
  map (prData $ mkDerivClause $ map mkName $ derivations cf0) $ cf2data cf0
  where
    mkDerivClause :: [Name] -> [Q DerivClause]
    mkDerivClause names = return $ return $ DerivClause Nothing (map ConT names)


absTokens :: CF -> Q [Dec]
absTokens cf0 = sequence $ 
  map (prSpecialData (mkDerivClause $ map mkName $ derivations cf0) cf0) (specialCats cf0)
  where
    mkDerivClause :: [Name] -> [DerivClause]
    mkDerivClause names = return $ DerivClause Nothing (map ConT names)


fixname :: String -> TypeQ
fixname ('[':xs) = appT listT $ conT $ mkName $ init xs
fixname xs = conT $ mkName xs
  
prData :: [DerivClauseQ] -> Data -> Q Dec
prData deriv (cat,rules) = 
  dataD (return []) (mkName cat) [] Nothing (map cons rules) deriv where
    cons (fun,cats) = normalC (mkName fun) $ either (map typ) (const str) cats
    typ = strictType notStrict . fixname
    str = [typ "String"]

-- deriv = [''Eq,''Ord,''Show]

prSpecialData :: [DerivClause] -> CF -> Cat -> Q Dec
prSpecialData deriv cf cat = do
  let con = normalC (mkName cat) $ [typ]
      typ = strictType notStrict $ contentSpec cf cat
  ctxt1 <- (return [])
  con1  <- con
  return (NewtypeD ctxt1 (mkName cat) [] Nothing con1 deriv)  
-- got rid of newtypeD by replacing it with its definition in 2.10.0.0, and then
-- forcing the parameters of NewtypeD to match up with the new types in 2.12.0.0


contentSpec :: CF -> Cat -> Q Type
contentSpec cf cat = if isPositionCat cf cat 
  then [t|((Int,Int),String)|] 
  else [t|String|]


-- aqName :: Bool -> String -> Name
-- aqName False s = 


-- transl cf = return []

-- lifts cf = return []
