module Parser (parse) where

import Types
import Data.Char
import Control.Monad.Instances

parse :: String -> Either String [Token]
parse [] = Right []
parse s@(']':_) = Left "Syntax error - unexpected right bracket"
parse s@(')':_) = Left "Syntax error - unexpected right paren"
parse s@('[':_) = do
	let (l,r) = depth '[' ']' s
	pl <- parse l
	pr <- parse r
	return ((LTok . F . Block$pl):pr)
parse s@('(':_) = do
	let (l,r) = depth '(' ')' s
	pr <- parse r
	return ((LTok. S $ l):pr)
parse s@('"':_) = case (reads::ReadS String) s of
	[] -> Left "Could not find end of string!"
	(a,b):_ -> do
		pr <- parse b
		return ((LTok . S $a):pr)	
parse s@(c:cs)
	| isSpace c = parse (dropWhile isSpace s)
	| isDigit c = let [(a,b)]=(reads::ReadS Double) s in do
		let n=LTok. N. toRational$a
		pr  <- parse b
		return (n:pr)
	| isAlpha c = let (a,b) = span isAlpha s in do
		let f=FTok a
		pr <- parse b
		return (f:pr)
	| otherwise = do
		let f=(FTok (c:[]))
		pr <- parse cs
		return (f:pr)
parse _ = error "Something bad happened... How did I get here?"

depth :: Char -> Char -> String -> (String,String)
depth u d s = let {
	helper y x | x==u = y+1
                   | x==d = y-1
                   | 2>1  = y;
	(_:l) = zip s (scanl helper 0 s);
	(r,rs) = break ((0==).snd) l;
} in (map fst $ init r, map fst rs)

