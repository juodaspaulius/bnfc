{-
    BNF Converter: Haskell error monad
    Copyright (C) 2004-2007  Author:  Markus Forberg, Peter Gammie,
                                      Aarne Ranta, Björn Bringert

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
module BNFC.Backend.Haskell.MkErrM where

errM :: String -> Bool -> String -> b -> String
errM errMod pos absMod _ = unlines
	   [
	    "-- BNF Converter: Error Monad",
	    "-- Copyright (C) 2004  Author:  Aarne Ranta",
	    "",
	    "-- This file comes with NO WARRANTY and may be used FOR ANY PURPOSE.",
	    "module " ++ errMod ++ " where",
	    "",
	    "-- the Error monad: like Maybe type with error msgs",
	    "",
      "import Control.Monad (MonadPlus(..), liftM)",
      "import Control.Applicative (Applicative(..), Alternative(..))",
      "",
      if pos then "import " ++ absMod else "",
      "",
	    "data Err a = Ok a | Bad" ++ (if pos then " Pos" else "") ++ " String",
	    "  deriving (Read, Show, Eq, Ord)",
	    "",
	    "instance Monad Err where",
	    "  return      = Ok",
	    (if pos then
      "  fail        = Bad noPos"
	    else
      "  fail        = Bad"),
	    "  Ok a  >>= f = f a",
      (if pos then
      "  Bad p s >>= _ = Bad p s"
      else
      "  Bad s >>= _ = Bad s"),
      "",
      "instance Applicative Err where",
      "  pure = Ok",
      (if pos then
      "  (Bad p s) <*> _ = Bad p s"
      else
      "  (Bad s) <*> _ = Bad s"),
      "  (Ok f) <*> o  = liftM f o",
      "",
      "",
      "instance Functor Err where",
      "  fmap = liftM",
      "",
      "instance MonadPlus Err where",
      (if pos then
      "  mzero = Bad noPos \"Err.mzero\""
      else
      "  mzero = Bad \"Err.mzero\""),
      (if pos then
      "  mplus (Bad _ _) y = y"
      else
      "  mplus (Bad _) y = y"),
      "  mplus x       _ = x",
      "",
      "instance Alternative Err where",
      "  empty = mzero",
      "  (<|>) = mplus"
	   ]
