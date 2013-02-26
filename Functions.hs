module Functions (default_funcs) where

import Types
import Interpreter
import Data.List
import Data.Function
import Data.Char
import qualified Data.Map as M

type SEFunc = InterpreterState -> InterpreterState
type  PFunc = Stack -> Stack

liftSE :: PFunc -> SEFunc
liftSE a = \r -> let b=stack r in r {stack = a b}

pToNF :: PFunc -> Function
pToNF = NFunc . liftSE

notEnoughArgs :: [a] -> Integer -> String -> b
notEnoughArgs argList expN fname = 
	error$"Not enough arguments to "++fname++"\n\tExpected: "++show expN++"\n\tReceived: "++show(length argList)

__int :: (Integral a) => Rational -> a
__int x = floor$((fromRational x)::Double)

default_funcs :: [(String, Function)]
default_funcs =  [
	("+", pToNF nf_plus),
	("-", pToNF nf_minus),
	("*", pToNF nf_times),
	("/", pToNF nf_div),
	("_", pToNF nf_neg),
	("#", pToNF nf_index),
	("~", pToNF nf_swap),
	("\\", pToNF nf_over),
	("`", pToNF nf_pop),
	("$", pToNF nf_dup),
	(":", pToNF nf_nil),
	("@", pToNF nf_rot),
	(".", pToNF nf_cons),
	(",", pToNF nf_uncons),
	("{", pToNF nf_lbrace),
	("}", pToNF nf_rbrace),
	("&", pToNF nf_append)]

-- symbol +
nf_plus :: PFunc
nf_plus (a:b:xs) = (__plus b a):xs
nf_plus e        = notEnoughArgs e 2 "plus"

__plus :: Data -> Data -> Data
__plus   Nil     _     = Nil
__plus   _       Nil   = Nil
__plus   (N a)   (N b) = N (a+b)
__plus n@(N a)   (C b) = C (map (__plus n) b)
__plus b@(C _) a@(N _) = __plus a b
__plus   (C x)   (C y) = C (zipWith __plus x y)
__plus   _       _     = Undef


-- symbol -
nf_minus :: PFunc
nf_minus (a:b:xs) = (__minus b a):xs
nf_minus e                = notEnoughArgs e 2 "minus"

__minus :: Data -> Data -> Data
__minus   Nil     _     = Nil
__minus   _       Nil   = Nil
__minus   (N a)   (N b) = N (a-b)
__minus n@(N a)   (C b) = C (map (__minus n) b)
__minus b@(C _) a@(N _) = __minus a b
__minus   (C x)   (C y) = C (zipWith __minus x y)
__minus   _       _     = Undef


-- symbol *
nf_times :: PFunc
nf_times (a:b:xs) = (__times b a):xs
nf_times e        = notEnoughArgs e 2 "times"

__times :: Data -> Data -> Data
__times   Nil     _     = Nil
__times   _       Nil   = Nil
__times   (N a)   (N b) = N (a*b)
__times n@(N a)   (C b) = C (map (__times n) b)
__times b@(C _) a@(N _) = __times a b
__times   (C x)   (C y) = C (zipWith __times x y)
__times   (S x)   (N y) = S (concat$replicate (__int y) x)
__times y@(N _) x@(S _) = __times x y
__times   (C x) s@(S y) = C (map (__times s) x)
__times y@(S _) x@(C _) = __times x y
__times   _       _     = Undef

-- symbol /
nf_div :: PFunc
nf_div (a:b:xs) = (__div b a):xs

__div   Nil     _     = Nil
__div   _       Nil   = Nil
__div   (N a)   (N b) = N (a/b)
__div   (C a) x@(N b) = C (map (`__div` x) a)
__div x@(N a)   (C b) = C (map (__div x) b)
__div   (C a)   (C b) = C (zipWith __div a b)
__div   _       _     = Undef

-- symbol _
nf_neg :: PFunc
nf_neg (a:xs) = (__neg a):xs
nf_neg _      = notEnoughArgs [] 1 "neg"

__neg (N x) = N (negate x)
__neg (C x) = C$map __neg x
__neg Nil   = Nil
__neg _     = Undef

-- symbol #
nf_index :: PFunc
nf_index (a:b:xs) = (__index b a):xs
nf_index e        = notEnoughArgs e 2 "index"

__index (C a) (N i)  = a!!!(__int i)
__index (C a) (C is) = C (map (__index (C a)) is)
__index a     b      = a

(!!!) :: (Integral n) => [Data] -> n -> Data
(!!!) []    _ = Nil
(!!!) (a:as) b | b<0 = Nil
               | b>0 = as !!! (b-1)
               | 2>1 = a

-- symbol ~
nf_swap :: PFunc
nf_swap (a:b:xs) = b:a:xs
nf_swap e        = notEnoughArgs e 2 "swap"

-- symbol \
nf_over :: PFunc
nf_over (a:b:xs) = b:a:b:xs
nf_over c      = notEnoughArgs c 2 "over"

--symbol `
nf_pop  :: PFunc
nf_pop (_:xs) = xs
nf_pop _    = error "Stack underflow on pop"

-- symbol $
nf_dup  :: PFunc
nf_dup (a:b) = a:a:b
nf_dup _     = notEnoughArgs [] 1 "dup"

-- symbol :
nf_nil  :: PFunc
nf_nil a   = Nil:a

-- symbol @
nf_rot  :: PFunc
nf_rot (a:b:c:xs) = b:c:a:xs
nf_rot e          = notEnoughArgs e 3 "rotate"

-- symbol .
nf_cons :: PFunc
nf_cons (a:(C b):xs) = (C (a:b)):xs
nf_cons (a:(Nil):xs) = (C$if a==Nil then [] else [a]):xs
nf_cons (a:b:xs)     = (C [a,b]):xs
nf_cons e            = notEnoughArgs e 2 "cons"

-- symbol ,
nf_uncons :: PFunc
nf_uncons ((C []):xs)   = Nil:Nil:xs
nf_uncons ((C [x]):xs)  = x:Nil:xs
nf_uncons ((C (d:ds)):xs) = d:(C ds):xs
nf_uncons (a:xs)        = a:Nil:xs
nf_uncons e             = notEnoughArgs [] 1 "uncons"

-- symbol &
nf_append :: PFunc
nf_append ((C x):(C y):zs) = (C (y ++ x)):zs
nf_append ((S x):(S y):zs) = (S (y ++ x)):zs
nf_append (  a  :(C y):zs) = (C (a:y)):zs
nf_append (  a  :(S y):zs) = (S (y++(show a))):zs
nf_append ((F (Block x)):(F (Block y)):zs) = (F (Block (x ++ y))):zs
nf_append ((F (NFunc x)):(F (NFunc y)):zs) = (F (NFunc (x . y))):zs
nf_append (_:_:z)=Undef:z
nf_append e                = notEnoughArgs e 2 "append"

-- symbol ;
nf_nop :: PFunc
nf_nop = id

-- symbol a
--nf_a :: PFunc
--nf_a x:y:xs

-- symbol S
nf_sort :: PFunc
nf_sort ((C a):xs) = (C (sort a)):xs
nf_sort (a    :xs) = a:xs
nf_sort _          = notEnoughArgs [] 1 "Sort"

-- symbol {
nf_lbrace :: PFunc
nf_lbrace = (Lbrace:)

-- symbol }
nf_rbrace :: PFunc
nf_rbrace s = let {
	(l,r)=span (/=Lbrace) s;
} in if null r then error "No matching left brace" else (C (reverse l)):(tail r)

main :: IO ()
main = do {
	print $ __times (C [N (3%1), N (2%5)]) (N (5%2));
	print $ __index (C [N (1%1), N (2%1), N (3%1), N (4%1), N (5%1)]) (C [N (negate (5%2)), N (1%1)]);
}
