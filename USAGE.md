# Typed HOL
A fully Typed HOL Language Definition using GATs, supporting the TPTP syntax and the Leo3 theorem prover.

## First steps 
### install ghci
[TBD]

### install leo3
[TBD]

### add leo3 to path as "leo3"
[TBD]

### first run
'''
$ cabal configure
'''
[TBD] [.. install packages]

'''
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
'''

Print the problem (with type)
'''
*SYO016_1> prettyTyped formulae
[ h :: Bool -> Bool
, leibeq: \x. \y. ∀p: ((p@x) -> (p@y)) :: Bool -> Bool -> Bool ]
'''

Print the conjecture (with type)
'''
*SYO016_1> prettyTyped conjecture
((leibeq@(h@((leibeq@(h@T))@(h@F))))@(h@F)) :: Bool
'''

Call leo3 to prover the conjecture given the 
'''
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
'''
