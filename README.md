# Typed HOL in Haskell
A fully Typed HOL Language Definition using GADTs, supporting the TPTP syntax and the Leo3 theorem prover.

## HOL using GADTs
We are using Generalized Abstract Data Types to model HOLTerms. 
This allows us to encode the type of a HOLTerm directly into it's Haskell type.

Noteable are:
```
  Lam :: (Typeable s, Typeable t, Typeable u) => HOLVar s u -> HOLTerm t u -> HOLTerm (s -> t) u
  App :: (Typeable s, Typeable t, Typeable u) => HOLTerm (s -> t) u -> HOLTerm s u -> HOLTerm t u

  f .@ x = App (toHOL f) (toHOL x)
  lam v a = Lam v (toHOL a)
```

Which allows constuction of new terms and the automatic deriving of their types:
```
> x = var "x" :: HOLVar Bool ()
> y = var "y" :: HOLVar Bool ()
> f = var "f" :: HOLVar (Bool -> Bool) ()

> f .@ x
(f@x) :: Bool

> lam x f
\x. f :: Bool -> Bool -> Bool

> lam y $ f .@ x
\y. (f@x) :: Bool -> Bool

> lam x y
\x. y :: Bool -> Bool
```

But fails on typelevel if you try to build terms which missmatching types:
```
> x .@ y
[..]: error:
    • Couldn't match type ‘Bool’ with ‘Bool -> t’
      Expected type: HOLVar (Bool -> t) ()
        Actual type: HOLVar Bool ()
    • In the first argument of ‘(.@)’, namely ‘x’
      In the expression: x .@ y
      In an equation for ‘it’: it = x .@ y
    • Relevant bindings include
        it :: HOLTerm t ()

> (f .@ x) .@ y
[..]: error:
    • Couldn't match type ‘Bool’ with ‘Bool -> t’
      Expected type: HOLTerm (Bool -> t) ()
        Actual type: HOLTerm Bool ()
    • In the first argument of ‘(.@)’, namely ‘(f .@ x)’
      In the expression: (f .@ x) .@ y
      In an equation for ‘it’: it = (f .@ x) .@ y
    • Relevant bindings include
        it :: HOLTerm t ()
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

Print the problem (with type)
```
*SYO016_1> formulae
[h :: Bool -> Bool,leibeq: \x. \y. ∀p: ((p@x) -> (p@y)) :: Bool -> Bool -> Bool]
```

Print the conjecture (with type)
```
*SYO016_1> conjecture
((leibeq@(h@((leibeq@(h@T))@(h@F))))@(h@F)) :: Bool
```

Call leo3 to prover the conjecture given the formulae
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
