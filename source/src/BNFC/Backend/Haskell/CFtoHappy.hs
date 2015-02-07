{-
    BNF Converter: Happy Generator
    Copyright (C) 2004  Author:  Markus Forberg, Aarne Ranta

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

module BNFC.Backend.Haskell.CFtoHappy (cf2HappyS, convert) where

import BNFC.CF
import BNFC.Backend.Common.StrUtils (escapeChars)
import BNFC.Backend.Haskell.Utils (parserName, catToType)
--import Lexer
import Data.Char
import BNFC.Options (HappyMode(..),SharedOptions(..))
import BNFC.PrettyPrint
-- Type declarations

type Rules       = [(NonTerminal,[(Pattern,Action)])]
type Pattern     = String
type Action      = String
type MetaVar     = String

-- default naming

tokenName   = "Token"

-- Happy mode



cf2HappyS :: String     -- ^ This module's name
          -> String     -- ^ Abstract syntax module name
          -> String     -- ^ Lexer module name
          -> String     -- ^ ErrM module name
          -> SharedOptions -- ^ Options
          -> CF         -- ^ Grammar
          -> String     -- ^ Generated code
---- cf2HappyS :: String -> CF -> String
cf2HappyS = cf2Happy

-- The main function, that given a CF and a CFCat to parse according to,
-- generates a happy module.
cf2Happy name absName lexName errName opts cf
 = unlines
    [header name absName lexName errName mode byteStrings',
     render $ declarations mode (allEntryPoints cf),
     tokens (cfTokens cf),
     specialToks cf,
     delimiter,
     specialRules byteStrings' cf,
     render $ prRules functor' (rulesForHappy functor' pos' cf),
     finalize byteStrings' pos' cf]
  where
    mode = glr opts
    byteStrings' = byteStrings opts
    functor' = functor opts
    pos' = positionsInAST opts

-- construct the header.
header :: String -> String -> String -> String -> HappyMode -> Bool -> String
header modName absName lexName errName mode byteStrings = unlines
         ["-- This Happy file was machine-generated by the BNF converter",
	  "{",
	  "{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}",
          case mode of
	    Standard -> "module " ++ modName ++ " where"
	    GLR      -> "-- module name filled in by Happy",
          "import " ++ absName,
          "import " ++ lexName,
          "import " ++ errName,
          if byteStrings then "import qualified Data.ByteString.Char8 as BS" else "",
          "}"
         ]

-- | The declarations of a happy file.
-- >>> declarations Standard [Cat "A", Cat "B", ListCat (Cat "B")]
-- %name pA A
-- %name pB B
-- %name pListB ListB
-- -- no lexer declaration
-- %monad { Err } { thenM } { returnM }
-- %tokentype {Token}
declarations :: HappyMode -> [Cat] -> Doc
declarations mode ns = vcat
    [ vcat $ map generateP ns
    , case mode of
        Standard -> "-- no lexer declaration"
        GLR      -> "%lexer { myLexer } { Err _ }",
      "%monad { Err } { thenM } { returnM }",
      "%tokentype" <+> braces (text tokenName) ]
  where generateP n = "%name" <+> parserName n <+> text n'
          where n' = identCat n

-- The useless delimiter symbol.
delimiter :: String
delimiter = "\n%%\n"

-- Generate the list of tokens and their identifiers.
tokens :: [(String,Int)] -> String
tokens toks = "%token\n" ++ prTokens toks
 where prTokens []         = []
       prTokens ((t,k):tk) = "  " ++ render (convert t) ++
                             " { " ++ oneTok t k ++ " }\n" ++
                             prTokens tk
       oneTok _ k = "PT _ (TS _ " ++ show k ++ ")"

-- Happy doesn't allow characters such as åäö to occur in the happy file. This
-- is however not a restriction, just a naming paradigm in the happy source file.
convert :: String -> Doc
convert = quotes . text . escapeChars

rulesForHappy :: Bool -> Bool -> CF -> Rules
rulesForHappy functor pos cf = map mkOne $ ruleGroups cf
  where
    mkOne (cat,rules) = (cat, map (constructRule functor pos reversibles cat) rules)
    reversibles = reversibleCats cf

-- | For every non-terminal, we construct a set of rules. A rule is a sequence
-- of terminals and non-terminals, and an action to be performed
-- >>> constructRule False [] (Rule "EPlus" (Cat "Exp") [Left (Cat "Exp"), Right "+", Left (Cat "Exp")])
-- ("Exp '+' Exp","EPlus $1 $3")
--
-- If we're using functors, it adds an void value:
-- >>> constructRule True [] (Rule "EPlus" (Cat "Exp") [Left (Cat "Exp"), Right "+", Left (Cat "Exp")])
-- ("Exp '+' Exp","EPlus () $1 $3")
--
-- Coercion are much simpler:
-- >>> constructRule True [] (Rule "_" (Cat "Exp") [Right "(", Left (Cat "Exp"), Right ")"])
-- ("'(' Exp ')'","$2")
--
-- As an optimization, a pair of list rules [C] ::= "" | C k [C] is
-- left-recursivized into [C] ::= "" | [C] C k.
-- This could be generalized to cover other forms of list rules.
-- >>> constructRule False [ListCat (Cat "A")] (Rule "(:)" (ListCat (Cat "A")) [Left (Cat "A"), Right",", Left (ListCat (Cat "A"))])
-- ("ListA A ','","flip (:) $1 $2")
--
-- Note that functors don't concern list constructors:
-- >>> constructRule True [ListCat (Cat "A")] (Rule "(:)" (ListCat (Cat "A")) [Left (Cat "A"), Right",", Left (ListCat (Cat "A"))])
-- ("ListA A ','","flip (:) $1 $2")
constructRule :: Bool -> Bool -> [Cat] -> NonTerminal -> Rule -> (Pattern,Action)
constructRule functor pos revs nt r0@(Rule fun cat _) = (pattern, action)
  where
    (pattern,metavars) = generatePatterns pos revs nt r
    action | isCoercion fun                 = unwords metavars
           | isConsFun fun && elem cat revs = unwords ("flip" : fun : metavars)
           | isNilCons fun                  = unwords (underscore fun : metavars)
           | functor                        = unwords (underscore fun : "()" : metavars)
           | otherwise                      = unwords (underscore fun : metavars)
    r | isConsFun (funRule r0) && elem (valCat r0) revs = revSepListRule r0
      | otherwise                                       = r0
    underscore f | isDefinedRule f = f ++ "_"
                 | otherwise       = f

-- Generate patterns and a set of metavariables indicating
-- where in the pattern the non-terminal

generatePatterns :: Bool -> [Cat] -> NonTerminal -> Rule -> (Pattern,[MetaVar])
generatePatterns pos revs nt r = case rhsRule r of
  []  -> ("{- empty -}",[prMetaPos []])
  its -> (unwords (map mkIt its), metas its)
 where
   mkIt i = case i of
     Left c -> identCat c
     Right s -> render (convert s)
   metas its = prMetaPos its : [revIf c ('$': show i) | (i,Left c) <- zip [1 ::Int ..] its]
   revIf c m = if not (isConsFun (funRule r)) && elem c revs
                 then "(reverse " ++ m ++ ")"
               else m  -- no reversal in the left-recursive Cons rule itself
   prMetaPos :: [Either Cat String] -> String
   prMetaPos its = if not pos || isList nt || isCoercion (funRule r) then "" else render $ concatPos its
   concatPos [] = text "noSpan"
   concatPos its = parens $ hsep (punctuate " >-" $ map mkSinglePos $ zip [1 ::Int ..] its)
   mkSinglePos (idx, item) = case item of
     Left _ -> parens $ "mkCatSpan $" <> text (show idx) -- Category
     Right _ -> parens $ "mkTokenSpan $" <> text (show idx) -- Token

-- We have now constructed the patterns and actions,
-- so the only thing left is to merge them into one string.

-- |
-- >>> prRules False [(Cat "Expr", [("Integer", "EInt $1"), ("Expr '+' Expr", "EPlus $1 $3")])]
-- Expr :: { Expr }
-- Expr : Integer { EInt $1 } | Expr '+' Expr { EPlus $1 $3 }
--
-- if there's a lot of cases, print on several lignes:
-- >>> prRules False [(Cat "Expr", [("Abcd", "Action"), ("P2", "A2"), ("P3", "A3"), ("P4", "A4"), ("P5","A5")])]
-- Expr :: { Expr }
-- Expr : Abcd { Action }
--      | P2 { A2 }
--      | P3 { A3 }
--      | P4 { A4 }
--      | P5 { A5 }
--
-- >>> prRules False [(Cat "Internal", [])] -- nt has only internal use
-- <BLANKLINE>
--
-- The functor case:
-- >>> prRules True [(Cat "Expr", [("Integer", "EInt () $1"), ("Expr '+' Expr", "EPlus () $1 $3")])]
-- Expr :: { (Expr ()) }
-- Expr : Integer { EInt () $1 } | Expr '+' Expr { EPlus () $1 $3 }
--
-- A list with coercion: in the type signature we need to get rid of the
-- coercion
-- >>> prRules True [(ListCat (CoercCat "Exp" 2), [("Exp2", "(:[]) $1"), ("Exp2 ',' ListExp2","(:) $1 $3")])]
-- ListExp2 :: { [Exp ()] }
-- ListExp2 : Exp2 { (:[]) $1 } | Exp2 ',' ListExp2 { (:) $1 $3 }
prRules :: Bool -> Rules -> Doc
prRules functor = vcat . map prOne
  where
    type' = catToType (if functor then Just "()" else Nothing)
    prOne (_,[]) = empty -- nt has only internal use
    prOne (nt,(p,a):ls) =
      hsep [ nt', "::", "{", type' nt, "}" ]
      $$ nt' <+> sep (pr ":" (p, a) : map (pr "|") ls)
     where
       nt' = text (identCat nt)
       pr pre (p,a) = hsep [pre, text p, "{", text a , "}"]

-- Finally, some haskell code.

finalize :: Bool -> Bool -> CF -> String
finalize byteStrings pos cf = unlines $
   [
     "{",
     "\nreturnM :: a -> Err a",
     "returnM = return",
     "\nthenM :: Err a -> (a -> Err b) -> Err b",
     "thenM = (>>=)",
     "\nhappyError :: [" ++ tokenName ++ "] -> Err a",
     "happyError ts =",
     "  " ++ (if pos then "Bad (pp ts)" else "Bad") ++
     " $ \"syntax error at \" ++ tokenPos ts ++ ",
     "  case ts of",
     "    [] -> []",
     "    [Err _] -> \" due to lexer error\"",
     "    _ -> \" before \" ++ unwords (map ("++stringUnpack++" . prToken) (take 4 ts))",
     "",
     "myLexer = tokens"
   ] ++ (if pos then posFunctions else []) ++ definedRules cf ++ [ "}" ]

   where
    stringUnpack
       | byteStrings = "BS.unpack"
       | otherwise   = "id"
    posFunctions :: [String]
    posFunctions =
      [ ""
      , "gp x@(PT (Pn _ l c) _) = Span (Pos (toInteger l) (toInteger c)) (Pos (toInteger l) (toInteger c + toInteger (length $ prToken x)))"
      , "pp (PT (Pn _ l c) _ :_) = Pos (toInteger l) (toInteger c)"
      , "pp (Err (Pn _ l c) :_) = Pos (toInteger l) (toInteger c)"
      , "pp _ = error \"EOF\"" -- End of file. What to do here?
      , ""
      , "mkCatSpan :: (Spannable c) => c -> Span"
      , "mkCatSpan = getSpan"
      , ""
      , "mkTokenSpan :: Token -> Span"
      , "mkTokenSpan = gp"
      ]


definedRules cf = [ mkDef f xs e | FunDef f xs e <- pragmasOfCF cf ]
    where
	mkDef f xs e = unwords $ (f ++ "_") : xs' ++ ["=", show e']
	    where
		xs' = map (++"_") xs
		e'  = underscore e
	underscore (App x es)
	    | isLower $ head x	= App (x ++ "_") $ map underscore es
	    | otherwise		= App x $ map underscore es
	underscore e	      = e

-- aarne's modifs 8/1/2002:
-- Markus's modifs 11/02/2002

-- GF literals
specialToks :: CF -> String
specialToks cf = unlines $
		 (map aux (literals cf))
 where aux cat =
        case show cat of
          "Ident"  -> "L_ident  { PT _ (TV $$) }"
          "String" -> "L_quoted { PT _ (TL $$) }"
          "Integer" -> "L_integ  { PT _ (TI $$) }"
          "Double" -> "L_doubl  { PT _ (TD $$) }"
          "Char"   -> "L_charac { PT _ (TC $$) }"
          own      -> "L_" ++ own ++ " { PT _ (T_" ++ own ++ " " ++ posn ++ ") }"
         where
           posn = if isPositionCat cf cat then "_" else "$$"

specialRules :: Bool -> CF -> String
specialRules byteStrings cf = unlines $
                  map aux (literals cf)
 where
   aux cat =
     case show cat of
         "Ident"   -> "Ident   :: { Ident }   : L_ident  { Ident $1 }"
	 "String"  -> "String  :: { String }  : L_quoted { "++stringUnpack++" $1 }"
	 "Integer" -> "Integer :: { Integer } : L_integ  { (read ("++stringUnpack++" $1)) :: Integer }"
	 "Double"  -> "Double  :: { Double }  : L_doubl  { (read ("++stringUnpack++" $1)) :: Double }"
	 "Char"    -> "Char    :: { Char }    : L_charac { (read ("++stringUnpack++" $1)) :: Char }"
	 own       -> own ++ "    :: { " ++ own ++ "} : L_" ++ own ++ " { " ++ own ++ " ("++ posn ++ "$1)}"
		-- PCC: take "own" as type name? (manual says newtype)
      where
         posn = if isPositionCat cf cat then "mkPosToken " else ""

   stringUnpack
     | byteStrings = "BS.unpack"
     | otherwise   = ""

