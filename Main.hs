module Main (main) where

import Interpreter
import System.Environment
import System.Random
import System.IO
import Functions

main = do {
	a <- getArgs;
	expression <- if (null a) then getLine else (return (head a));
	rg <- getStdGen;
	ip <- hGetContents stdin;
	bstate <- return$InterpreterState {
		input = ip,
		stack = [],
		output = [],
		rgen = rg,
		dict = Functions.default_funcs
	};
	state2 <- return$doString expression bstate;
	putStr.concat.reverse$output state2;
	putStr.unwords.reverse.map show$stack state2;
}

