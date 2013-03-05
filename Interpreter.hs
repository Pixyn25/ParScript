module Interpreter (runTok, runToks, doString, module Types) where

import Types
import Parser

runTok :: Token -> InterpreterState -> InterpreterState
runTok (CTok _) b = b
runTok (LTok a) b = let s=stack b in b {stack = a:s}
runTok (FTok a) b = let f=lookup a (dict b) in case f of
	Just x -> case x of
		Block t -> runToks t b
		NFunc n -> n b
	Nothing -> b { output = ("Unknown function: "++a++"\n"):(output b) }

runToks :: [Token] -> InterpreterState -> InterpreterState
runToks = flip (foldl (flip runTok))

doString :: String -> InterpreterState -> InterpreterState
doString a s = case (parse a) of
	Right ts -> runToks ts s
	Left  e  -> s { output = ("Cannot eval: "++e++"!\n"):(output s) }
