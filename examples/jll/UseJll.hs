{-# LANGUAGE QuasiQuotes #-}
import JavaletteLight
import Language.LBNF.Runtime -- overloaded pretty-printing function
import Prelude hiding (exp)

{- This Javalette Light program is parsed at compile time, 
and replaced by it's abstract syntax representation.
The 'holes' in square brackets are anti-quoted Haskell 
expression. 

The QuasiQuoter prog is generated from the grammar in JavaletteLight.hs
(it corresponds to the category Prog).
-}

printing' :: Expr -> Prog
printing' e = [prog| 
   int main (  ) { 
     print ( $e ) ; 
   } 
   |]

printing e = Fun TInt (Ident "main") [SFunApp (Ident "print") [e]]

print10 :: Prog
print10 = printing' [expr| 5 + 5 |]

