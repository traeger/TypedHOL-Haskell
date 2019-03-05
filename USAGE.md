$ ghci SYO016^1.lhs 
GHCi, version 8.2.2: http://www.haskell.org/ghc/  :? for help
[1 of 4] Compiling HOL              ( HOL.lhs, interpreted )
[2 of 4] Compiling TPTP_THF         ( TPTP_THF.lhs, interpreted )
[3 of 4] Compiling Leo3             ( Leo3.lhs, interpreted )
[4 of 4] Compiling SYO016_1         ( SYO016^1.lhs, interpreted )
Ok, four modules loaded.

*SYO016_1> valid problem conj
% [INFORMATION]      No timeout was given, using default timeout -t 60 
% Time passed: 3253ms
% Effective reasoning time: 2194ms
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
% SZS status Theorem for /dev/stdin : 3253 ms resp. 2194 ms w/o parsing

True