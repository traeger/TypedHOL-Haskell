%------------------------------------------------------------------------------
% File     : SYO016^1 : TPTP v7.2.0. Released v3.7.0.
% Domain   : Syntactic
% Problem  : Formula valid in MBb, but not in model classes not requiring b
% Version  : Especial.
% English  :

% Refs     : [BB05]  Benzmueller & Brown (2005), A Structured Set of Higher
%          : [Ben09] Benzmueller (2009), Email to Geoff Sutcliffe
% Source   : [Ben09]
% Names    : Example 18a [BB05]

% Status   : Theorem
%          : Without Boolean extensionality : CounterSatisfiable
% Rating   : 0.22 v7.2.0, 0.12 v7.1.0, 0.38 v7.0.0, 0.29 v6.4.0, 0.33 v6.3.0, 0.40 v6.2.0, 0.29 v6.1.0, 0.43 v5.5.0, 0.33 v5.4.0, 0.60 v5.1.0, 0.80 v4.1.0, 1.00 v4.0.1, 0.67 v3.7.0
% Syntax   : Number of formulae    :    4 (   0 unit;   2 type;   1 defn)
%            Number of atoms       :   15 (   1 equality;   4 variable)
%            Maximal formula depth :    7 (   5 average)
%            Number of connectives :   11 (   0   ~;   0   |;   0   &;  10   @)
%                                         (   0 <=>;   1  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&)
%            Number of type conns  :    4 (   4   >;   0   *;   0   +;   0  <<)
%            Number of symbols     :    5 (   2   :;   0   =)
%            Number of variables   :    3 (   0 sgn;   1   !;   0   ?;   2   ^)
%                                         (   3   :;   0  !>;   0  ?*)
%                                         (   0  @-;   0  @+)
% SPC      : TH0_THM_EQU_NAR

% Comments : 
%------------------------------------------------------------------------------
thf(leibeq_decl,type,(
    leibeq: $o > $o > $o )).

thf(leibeq,definition,
    ( leibeq
    = ( ^ [X: $o,Y: $o] :
        ! [P: $o > $o] :
          ( ( P @ X )
         => ( P @ Y ) ) ) )).

thf(h,type,(
    h: $o > $o )).

thf(conj,conjecture,
    ( leibeq @ ( h @ ( leibeq @ ( h @ $true ) @ ( h @ $false ) ) ) @ ( h @ $false ) )).

%------------------------------------------------------------------------------

\begin{code}
module SYO016_1 where

import Logic.HOL
import Logic.Prover.HOL.Leo3 as Leo3
import Prelude hiding (not, forall, exists)

leibeq =
  let
    x = var "x" :: HOLVar () Bool
    y = var "y" :: HOLVar () Bool
    p = var "p" -- :: HOLVar () (Bool -> Bool) <- autoderived
  in
    definition "leibeq" $ lam x $ lam y $ forall p $ p .@ x .-> p .@ y

h = constant "h" :: HOLConst () (Bool -> Bool)
conjecture = leibeq .@ ( h .@ ( leibeq .@ ( h .@ T ) .@ ( h .@ F ) ) ) .@ ( h .@ F )

formulae = 
  [ gen h
  , gen leibeq
  ]

g = Leo3.toTPTP formulae conjecture
f = Leo3.valid formulae conjecture
\end{code}