module Parser (parse) where

import Types
import Data.Char
import Control.Monad.Instances

parse :: String -> Either String [Token]
parse [] = Right []
parse s@(']':_) = Left "Unexpected right bracket"
parse s@(')':_) = Left "Unexpected right paren"
parse s@('[':_) = do
	let (l,r) = depth '[' ']' s
	pl <- parse l
	pr <- parse r
	return ((LTok . F . Block$pl):pr)
parse s@('(':_) = do
	let (l,r) = depth '(' ')' s
	pr <- parse r
	return ((CTok l):pr)
parse s@('"':r) = Left "Not yet implemented"
parse s@(c:cs)
	| isSpace c = parse (dropWhile isSpace s)
	| isDigit c = let [(a,b)]=(reads::ReadS Double) s in do
		let n=LTok. N. toRational$a
		pr  <- parse b
		return (n:pr)
	| otherwise = do
		let f=(FTok (c:[]))
		pr <- parse cs
		return (f:pr)
parse _ = Left "Not yet implemented"

depth :: Char -> Char -> String -> (String,String)
depth u d s = let {
	helper y x | x==u = y+1
                   | x==d = y-1
                   | 2>1  = y;
	(_:l) = zip s (scanl helper 0 s);
	(r,rs) = break ((0==).snd) l;
} in (map fst r, map fst rs)

