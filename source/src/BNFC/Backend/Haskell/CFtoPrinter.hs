{-
    BNF Converter: Pretty-printer generator
    Copyright (C) 2004  Author:  Aarne Ranta

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

module BNFC.Backend.Haskell.CFtoPrinter (cf2Printer, compareRules) where

import BNFC.Backend.Haskell.Utils (hsReservedWords)
import BNFC.CF
import BNFC.Utils
import Data.Char(toLower)
import Data.Either (lefts)
import Data.List (sortBy)
import Data.Maybe (fromJust)
import Text.PrettyPrint

-- derive pretty-printer from a BNF grammar. AR 15/2/2002
cf2Printer :: Bool -> Bool -> Bool -> Bool -> String -> String -> CF -> String
cf2Printer byteStrings functor useGadt pos name absMod cf = unlines [
  prologue byteStrings useGadt name absMod,
  integerRule cf,
  doubleRule cf,
  if hasIdent cf then identRule byteStrings cf else "",
  unlines [ownPrintRule byteStrings cf own | (own,_) <- tokenPragmas cf],
  rules functor pos cf
  ]


prologue :: Bool -> Bool -> String -> String -> String
prologue byteStrings useGadt name absMod = unlines $
  ["{-# LANGUAGE GADTs, TypeSynonymInstances, FlexibleInstances #-}" | useGadt]
  ++ [
  "{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}",
  "module " ++ name +++ "where\n",
  "-- pretty-printer generated by the BNF converter\n",
  "import " ++ absMod,
  "import Data.Char",
  (if byteStrings then "import qualified Data.ByteString.Char8 as BS" else ""),
  "",
  "-- the top-level printing method",
  "printTree :: Print a => a -> String",
  "printTree = render . prt 0",
  "",
  "type Doc = [ShowS] -> [ShowS]",
  "",
  "doc :: ShowS -> Doc",
  "doc = (:)",
  "",
  "render :: Doc -> String",
  "render d = rend 0 (map ($ \"\") $ d []) \"\" where",
  "  rend i ss = case ss of",
  "    \"[\"      :ts -> showChar '[' . rend i ts",
  "    \"(\"      :ts -> showChar '(' . rend i ts",
  "    \"{\"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts",
  "    \"}\" : \";\":ts -> new (i-1) . space \"}\" . showChar ';' . new (i-1) . rend (i-1) ts",
  "    \"}\"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts",
  "    \";\"      :ts -> showChar ';' . new i . rend i ts",
  "    t  : \",\" :ts -> showString t . space \",\" . rend i ts",
  "    t  : \")\" :ts -> showString t . showChar ')' . rend i ts",
  "    t  : \"]\" :ts -> showString t . showChar ']' . rend i ts",
  "    t        :ts -> space t . rend i ts",
  "    _            -> id",
  "  new i   = showChar '\\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace",
  "  space t = showString t . (\\s -> if null s then \"\" else (' ':s))",
  "",
  "parenth :: Doc -> Doc",
  "parenth ss = doc (showChar '(') . ss . doc (showChar ')')",
  "",
  "concatS :: [ShowS] -> ShowS",
  "concatS = foldr (.) id",
  "",
  "concatD :: [Doc] -> Doc",
  "concatD = foldr (.) id",
  "",
  "replicateS :: Int -> ShowS -> ShowS",
  "replicateS n f = concatS (replicate n f)",
  "",
  "-- the printer class does the job",
  "class Print a where",
  "  prt :: Int -> a -> Doc",
  "  prtList :: Int -> [a] -> Doc",
  "  prtList i = concatD . map (prt i)",
  "",
  "instance Print a => Print [a] where",
  "  prt = prtList",
  "",
  "instance Print Char where",
  "  prt _ s = doc (showChar '\\'' . mkEsc '\\'' s . showChar '\\'')",
  "  prtList _ s = doc (showChar '\"' . concatS (map (mkEsc '\"') s) . showChar '\"')",
  "",
  "mkEsc :: Char -> Char -> ShowS",
  "mkEsc q s = case s of",
  "  _ | s == q -> showChar '\\\\' . showChar s",
  "  '\\\\'-> showString \"\\\\\\\\\"",
  "  '\\n' -> showString \"\\\\n\"",
  "  '\\t' -> showString \"\\\\t\"",
  "  _ -> showChar s",
  "",
  "prPrec :: Int -> Int -> Doc -> Doc",
  "prPrec i j = if j<i then parenth else id",
  ""
  ]

integerRule cf = showsPrintRule cf "Integer"
doubleRule cf = showsPrintRule cf "Double"

showsPrintRule cf t = unlines $ [
  "instance Print " ++ t ++ " where",
  "  prt _ x = doc (shows x)",
  ifList cf (Cat t)
  ]

identRule byteStrings cf = ownPrintRule byteStrings cf catIdent

ownPrintRule byteStrings cf own = unlines $ [
  "instance Print " ++ show own ++ " where",
  "  prt _ (" ++ show own ++ posn ++ ") = doc (showString ("++stringUnpack++" i))",
  ifList cf own
  ]
 where
   posn = if isPositionCat cf own then " (_,i)" else " i"

   stringUnpack | byteStrings = "BS.unpack"
                | otherwise   = ""

-- copy and paste from BNFC.Backend.Haskell.CFtoTemplate

rules :: Bool -> Bool -> CF -> String
rules functor pos cf = unlines $

  map (\(s,xs) -> render (case_fun functor pos s (map toArgs xs)) ++++ ifList cf s) $ cf2data cf
 where
   toArgs (cons,_) = (cons, ruleOf cons)
   ruleOf s = fromJust $ lookupRule s (rulesOfCF cf)

-- |
-- >>> case_fun False (Cat "A") [("AA", (Cat "AB", [Right "xxx"]))]
-- instance Print A where
--   prt i e = case e of
--     AA -> prPrec i 0 (concatD [doc (showString "xxx")])
case_fun :: Bool -> Bool -> Cat -> [(String, (Cat, [Either Cat String]))] -> Doc
case_fun functor pos cat xs = vcat
    [ "instance Print" <+> type_ <+> "where"
    , nest 2 $ vcat
        [ "prt i e = case e of"
        , nest 2 $ vcat (map (mkPrintCase functor pos) xs)
        ]
    ]
  where
    type_ | functor   = parens (text (show cat) <+> "a")
          | otherwise = text (show cat)

--  When writing the Print instance for a category (in case_fun), we have
-- a different case for each constructor for this category.
-- >>> mkPrintCase False ("AA", (Cat "A", [Right "xxx"]))
-- AA -> prPrec i 0 (concatD [doc (showString "xxx")])
--
-- Coercion levels are passed to prPrec
-- >>> mkPrintCase False ("EInt", (CoercCat "Expr" 2, [Left (TokenCat "Integer")]))
-- EInt n -> prPrec i 2 (concatD [prt 0 n])
-- >>> mkPrintCase False ("EPlus", (CoercCat "Expr" 1, [Left (Cat "Expr"), Right "+", Left (Cat "Expr")]))
-- EPlus expr0 expr -> prPrec i 1 (concatD [prt 0 expr0, doc (showString "+"), prt 0 expr])
--
-- If the AST is a functor, ignore first argument
-- >>> mkPrintCase True ("EInt", (CoercCat "Expr" 2, [Left (TokenCat "Integer")]))
-- EInt _ n -> prPrec i 2 (concatD [prt 0 n])
--
-- Skip intertal categories
-- >>> mkPrintCase True ("EInternal", (Cat "Expr", [Left InternalCat, Left (Cat "Expr")]))
-- EInternal _ expr -> prPrec i 0 (concatD [prt 0 expr])
mkPrintCase :: Bool -> Bool -> (Fun, (Cat, [Either Cat String])) -> Doc
mkPrintCase functor pos (f, (cat, rhs)) =
    text f <+> (if pos then "_" else empty) <+> (if functor then "_" else empty) <+> hsep variables <+> "->"
    <+> "prPrec i" <+> integer (precCat cat) <+> mkRhs (map render variables) rhs
  where
    -- Creating variables names used in pattern matching. In addition to
    -- haskell's reserved words, `e` and `i` are used in the printing function
    -- and should be avoided
    names = map var (filter (/=InternalCat) $ lefts rhs)
    variables = map text $ mkNames ("e":"i":hsReservedWords) LowerCase names
    var (ListCat c)  = var c ++ "s"
    var (TokenCat "Ident")   = "id"
    var (TokenCat "Integer") = "n"
    var (TokenCat "String")  = "str"
    var (TokenCat "Char")    = "c"
    var (TokenCat "Double")  = "d"
    var xs        = map toLower $ show xs

ifList :: CF -> Cat -> String
ifList cf cat = render $ nest 2 $ vcat [ mkPrtListCase r | r <- rules ]
  where
    rules = sortBy compareRules $ rulesForNormalizedCat cf (ListCat cat)


-- | Pattern match on the list constructor and the coercion level
-- >>> mkPrtListCase (Rule "[]" (ListCat (Cat "Foo")) [])
-- prtList _ [] = (concatD [])
-- >>> mkPrtListCase (Rule "(:[])" (ListCat (Cat "Foo")) [Left (Cat "FOO")])
-- prtList _ [x] = (concatD [prt 0 x])
-- >>> mkPrtListCase (Rule "(:)" (ListCat (Cat "Foo")) [Left (Cat "Foo"), Left (ListCat (Cat "Foo"))])
-- prtList _ (x:xs) = (concatD [prt 0 x, prt 0 xs])
-- >>> mkPrtListCase (Rule "[]" (ListCat (CoercCat "Foo" 2)) [])
-- prtList 2 [] = (concatD [])
-- >>> mkPrtListCase (Rule "(:[])" (ListCat (CoercCat "Foo" 2)) [Left (CoercCat "Foo" 2)])
-- prtList 2 [x] = (concatD [prt 2 x])
-- >>> mkPrtListCase (Rule "(:)" (ListCat (CoercCat "Foo" 2)) [Left (CoercCat "Foo" 2), Left (ListCat (CoercCat "Foo" 2))])
-- prtList 2 (x:xs) = (concatD [prt 2 x, prt 2 xs])
mkPrtListCase :: Rule -> Doc
mkPrtListCase (Rule f (ListCat c) rhs)
  | isNilFun f = "prtList" <+> precPattern <+> "[]" <+> "=" <+> body
  | isOneFun f = "prtList" <+> precPattern <+> "[x]" <+> "=" <+> body
  | isConsFun f = "prtList" <+> precPattern <+> "(x:xs)" <+> "=" <+> body
  | otherwise = empty -- (++) constructor
  where
    precPattern = case precCat c of 0 -> "_" ; p -> integer p
    body = mkRhs ["x", "xs"] rhs


-- | Define an ordering on lists' rules with the following properties:
-- - rules with a higher coercion level should come first, i.e. the rules for
--   [Foo3] are before rules for [Foo1] and they are both lower than rules for
--   [Foo].
-- - [] < [_] < _:_
-- This is desiged to correctly order the rules in the prtList function so that
-- the pattern matching works as expectd.
--
-- >>> compareRules (Rule "[]" (ListCat (CoercCat "Foo" 3)) []) (Rule "[]" (ListCat (CoercCat "Foo" 1)) [])
-- LT
-- >>> compareRules (Rule "[]" (ListCat (CoercCat "Foo" 3)) []) (Rule "[]" (ListCat (Cat "Foo")) [])
-- LT
-- >>> compareRules (Rule "[]" (ListCat (Cat "Foo")) []) (Rule "(:[])" (ListCat (Cat "Foo")) [])
-- LT
-- >>> compareRules (Rule "(:[])" (ListCat (Cat "Foo")) []) (Rule "(:)" (ListCat (Cat "Foo")) [])
-- LT
compareRules :: Rule -> Rule -> Ordering
compareRules r1 r2 | precRule r1 > precRule r2 = LT
compareRules r1 r2 | precRule r1 < precRule r2 = GT
compareRules (Rule "[]" _ _) (Rule "[]" _ _) = EQ
compareRules (Rule "[]" _ _) _ = LT
compareRules (Rule "(:[])" _ _) (Rule "[]" _ _) = GT
compareRules (Rule "(:[])" _ _) (Rule "(:[])" _ _) = EQ
compareRules (Rule "(:[])" _ _) (Rule "(:)" _ _) = LT
compareRules (Rule "(:)" _ _) (Rule "(:)" _ _) = EQ
compareRules (Rule "(:)" _ _) _ = GT
compareRules _ _ = EQ


-- |
-- >>> mkRhs ["expr1", "n", "expr2"] [Left (Cat "Expr"), Right "-", Left (TokenCat "Integer"), Left (Cat "Expr")]
-- (concatD [prt 0 expr1, doc (showString "-"), prt 0 n, prt 0 expr2])
--
-- Coercions on the right hand side should be passed to prt:
-- >>> mkRhs ["expr1"] [Left (CoercCat "Expr" 2)]
-- (concatD [prt 2 expr1])
-- >>> mkRhs ["expr2s"] [Left (ListCat (CoercCat "Expr" 2))]
-- (concatD [prt 2 expr2s])
mkRhs :: [String] -> [Either Cat String] -> Doc
mkRhs args its =
    parens ("concatD" <+> brackets (hsep (punctuate "," (mk args its))))
 where
  mk args (Left InternalCat : items) = mk args items
  mk (arg:args) (Left c  : items)    = (prt c <+> text arg) : mk args items
  mk args       (Right s : items)    = ("doc (showString" <+> text (show s) <> ")") : mk args items
  mk _          _                    = []
  prt c = "prt" <+> integer (precCat c)

