ParScript
=========

Interpreter for the ParScript language as defined in http://esolangs.org/wiki/Parscript

The interpreter
---------------

The interpreter is written in Haskell and compiled using GHC. Currently, the interface is very minimal.

* ```--help``` and ```-h``` give the help options
* ```--file``` and ```-f``` give the name of a file to be executed
* ```--loop``` and ```-p``` causes interactive mode
* ```--auto``` and ```-a``` causes the line input by the user to be split first. Implies ```-p```.

Language features
-----------------

* Arithmetic on vectors - arithmetic operation of vectors
* Large amount of standard functions
* Defining of new functions at runtime

Sample code
-----------

```
(ncr function
 call as x y ncr = nCr(x,y) )

\\-i~i*P~iP~/

```

