ParScript
=========

Interpreter for the ParScript language as defined in http://esolangs.org/wiki/Parscript

The interpreter
---------------

The interpreter is written in Haskell and compiled using GHC. it can be easily imported into any Haskell application as a library. It supports the folowing options:

* ```--help``` and ```-h``` give the help options
* ```--file``` and ```-f``` give the name of a file to be executed
* ```--loop``` and ```-p``` causes interactive mode
* ```--auto``` and ```-a``` causes the line input by the user to be split first. Implies ```-p```.

Interpreter internals
---------------------

```Haskell
data InterpreterState = { input :: String, output :: [String], stack :: [Data] , dict :: Map.Map String Function}
data Data = N Real | S String | C Data Data | F Function | Nil | Undef | LStart
data Token = NTok Real | STok String | IDTok String | BTok [Token]
data Function = B [Token] | NFunc ([Data] -> [Data])

parse  :: String -> [Token]
doTok  :: InterpreterState -> Token -> InterpreterState
doToks :: InterpreterState -> [Token] -> InterpreterState
```

