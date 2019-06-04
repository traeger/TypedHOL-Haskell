# Typed HOL in Haskell
A fully Typed HOL Language Definition using GADTs, supporting the TPTP syntax and the Leo3 theorem prover.

## HOL using GADTs
We are using Generalized Abstract Data Types to model HOLTerms. 
This allows us to encode the type of a HOLTerm directly into it's Haskell type.

Noteable are:
```
  Lam :: (Typeable s, Typeable t, Typeable u) => HOLVar u s -> HOLTerm u t -> HOLTerm u (s -> t)
  App :: (Typeable s, Typeable t, Typeable u) => HOLTerm u (s -> t) -> HOLTerm u s -> HOLTerm u t

  f .@ x = App (toHOL f) (toHOL x)
  lam v a = Lam v (toHOL a)
```

Which allows constuction of new terms and the automatic deriving of their types:
```
> x = var "x" :: HOLVar () Bool
> y = var "y" :: HOLVar () Bool ()
> f = var "f" :: HOLVar () (Bool -> Bool)

> f .@ x
(f@x) :: Bool

> lam x f
\x. f :: Bool -> Bool -> Bool

> lam y $ f .@ x
\y. (f@x) :: Bool -> Bool

> lam x y
\x. y :: Bool -> Bool
```

But fails on typelevel if you try to build terms with missmatching types:
```
> x .@ y
[..]: error:
    • Couldn't match type ‘Bool’ with ‘Bool -> t’
      Expected type: HOLVar () (Bool -> t)
        Actual type: HOLVar () Bool
    • In the first argument of ‘(.@)’, namely ‘x’
      In the expression: x .@ y
      In an equation for ‘it’: it = x .@ y
    • Relevant bindings include
        it :: HOLTerm () t

> (f .@ x) .@ y
[..]: error:
    • Couldn't match type ‘Bool’ with ‘Bool -> t’
      Expected type: HOLTerm () (Bool -> t)
        Actual type: HOLTerm () Bool
    • In the first argument of ‘(.@)’, namely ‘(f .@ x)’
      In the expression: (f .@ x) .@ y
      In an equation for ‘it’: it = (f .@ x) .@ y
    • Relevant bindings include
        it :: HOLTerm () t
```

## First steps 
### install ghci
[TBD]

### install leo3
[TBD]

### add leo3 to path as "leo3"
[TBD]

### first run
```
$ cabal configure
```
[TBD] [.. install packages]

```
$ cd src
$ ghci 'SYO016^1.lhs' 
GHCi, version [...] :? for help
[1 of 6] Compiling Logic.HOL        ( Logic/HOL.lhs, interpreted )
[2 of 6] Compiling Logic.Prover.SZSStatus ( Logic/Prover/SZSStatus.lhs, interpreted )
[3 of 6] Compiling Logic.Prover     ( Logic/Prover.lhs, interpreted )
[4 of 6] Compiling Logic.TPTP.THF   ( Logic/TPTP/THF.lhs, interpreted )
[5 of 6] Compiling Logic.Prover.HOL.Leo3 ( Logic/Prover/HOL/Leo3.lhs, interpreted )
[6 of 6] Compiling SYO016_1         ( SYO016^1.lhs, interpreted )
Ok, six modules loaded.
```

The file SYO016^1.lhs contains the haskell formulation of the TPTP problem [SYO016^1](http://www.tptp.org/cgi-bin/SeeTPTP?Category=Problems&Domain=SYO&File=SYO016^1.p).
```
module SYO016_1 where

import Logic.HOL
import Logic.Prover.HOL.Leo3 as Leo3
import Prelude hiding (not, forall, exists)

leibeq =
  let
    x = var "x" :: HOLVar () (Bool) 
    y = var "y" :: HOLVar () (Bool) 
    p = var "p" -- :: HOLVar () (Bool -> Bool) <- autoderived
  in
    definition "leibeq" $ lam x $ lam y $ forall p $ p .@ x .-> p .@ y

h = constant "h" :: HOLConst () (Bool -> Bool)
conjecture = leibeq .@ ( h .@ ( leibeq .@ ( h .@ T ) .@ ( h .@ F ) ) ) .@ ( h .@ F )

formulae = 
  [ gen h
  , gen leibeq
  ]
```

Print the formulae (with types)
```
*SYO016_1> formulae
[h :: Bool -> Bool,leibeq: \x. \y. ∀p: ((p@x) -> (p@y)) :: Bool -> Bool -> Bool]
```

Print the conjecture (with type)
```
*SYO016_1> conjecture
((leibeq@(h@((leibeq@(h@T))@(h@F))))@(h@F)) :: Bool
```

Call leo3 to prove the conjecture given the formulae
```
*SYO016_1> Leo3.valid formulae conjecture
% [INFORMATION]      No timeout was given, using default timeout -t 60 
% Time passed: 3148ms
% Effective reasoning time: 2072ms
% Solved by strategy<name(default),share(1.0),primSubst(1),sos(false),unifierCount(1),uniDepth(8),boolExt(true),choice(true),renaming(true),funcspec(false), domConstr(0),specialInstances(-1),restrictUniAttempts(true)>
% Axioms used in derivation (0): 
% No. of processed clauses: 10
% No. of generated clauses: 157
% No. of forward subsumed clauses: 2
% No. of backward subsumed clauses: 0
% No. of ground rewrite rules in store: 4
% No. of non-ground rewrite rules in store: 0
% No. of positive (non-rewrite) units in store: 0
% No. of negative (non-rewrite) units in store: 2
% No. of choice functions detected: 0
% No. of choice instantiations: 0
% SZS status Theorem for /dev/stdin : 3148 ms resp. 2072 ms w/o parsing

Theorem
```

## THF Parser
### parse an TPTP-THF file
```
$ ghci Logic/TPTP/THF/AST.lhs
*Logic.TPTP.THF.AST> parseFile abstractTHFFile "../TPTP/SYO016^1.p"
AbstractTHFFile 
[ AbstractTHFType "leibeq_decl" "( leibeq: $o > $o > $o )"
, AbstractTHFDefinition "leibeq" "( leibeq = ( ^ [X: $o,Y: $o] : ! [P: $o > $o] : ( ( P @ X ) => ( P @ Y ) ) ) )"
, AbstractTHFType "h" "( h: $o > $o )"
, AbstractTHFConjecture "conj" "( leibeq @ ( h @ ( leibeq @ ( h @ $true ) @ ( h @ $false ) ) ) @ ( h @ $false ) )"
]
```
