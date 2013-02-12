module Parscript.Types(
	Stack,
	Data(..),
	Function(..),
	Token(..),
	module Prelude
) where

import Data.Ratio
import Prelude

type Stack = [Data]
data Data = N Rational | S String | C Data Data | F Function | Nil | Undef | Lbrace deriving (Eq)

data Function= Block [Token] | NFunc (Stack->Stack)

data Token = CTok String | NTok Rational | STok String | BTok [Token] deriving (Eq, Ord)

instance Eq Function where
	(==) (Block a) (Block b) = a == b
	(==) _         _         = False

instance Show Function where
	show (Block a) = '[':((unwords.map show$a)++"]")
	show (NFunc _) = "<native function>"

instance Show Token where
	show (CTok a) = a
	show (STok a) = show a
	show (NTok a) = show (N a)
	show (BTok b) = show (Block b)

instance Ord Data where
	Undef `compare` _ = undefined
	_ `compare` Undef = undefined
	Nil `compare` _   = LT
	_   `compare` Nil = GT
	Lbrace `compare`_ = LT
	_`compare` Lbrace = GT
	N a `compare` N b = a `compare` b
	N _ `compare` _   = LT
	S _ `compare` N _ = GT
	S a `compare` S b = a `compare` b
	S _ `compare` _   = LT
	F (Block x) `compare` F (Block y) = x `compare` y
	F _         `compare` F _         = undefined
	F _         `compare` _           = GT

instance Show Data where
	show (N x) = let {
	        	(a,b) = (numerator x) `divMod` (denominator x)
		} in if b==0 then show a else show ((fromRational x)::Double)
	show (S x) = x
	show (C x y) = '(':(f x y)
	show (F a) = show a
	show Nil   = "nil"
	show Undef = "undef"
	show Lbrace= "{"

f :: Data -> Data -> String
f x Nil       = (show x)++")"
f x (C y Nil) = (show x)++" "++(show y)++")"
f x (C y z)   = (show x)++" "++f y z
f x y         = (show x)++" . "++(show y)++")"


