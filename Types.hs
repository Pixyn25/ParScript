module Types(
	Stack,
	Data(..),
	Function(..),
	Token(..),
	InterpreterState(..),
	module Data.Ratio
) where

import Data.Ratio
import Prelude
import System.Random

type Stack = [Data]
data Data = N Rational | S String | C [Data] | F Function | Nil | Undef | Lbrace deriving (Eq)

data Function= Block [Token] | NFunc (InterpreterState -> InterpreterState)

data Token = CTok String | LTok Data | FTok String deriving (Eq, Ord)

data InterpreterState = InterpreterState {
        input  :: String,
        stack  :: Stack,
        output :: [String],
        rgen   :: StdGen,
        dict   :: [(String,Function)]
}

instance Eq Function where
	(==) (Block a) (Block b) = a == b
	(==) _         _         = False

instance Show Function where
	show (Block a) = '[':((unwords.map show$a)++"]")
	show (NFunc _) = "<native>"

instance Show Token where
	show (CTok a) = "("++a++")"
	show (LTok l) = show l
	show (FTok a) = a

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
	show (S x) = "("++x++")"
	show (C x) = '{':((unwords.map show$x)++"}")
	show (F a) = show a
	show Nil   = "nil"
	show Undef = "undef"
	show Lbrace= "{"


