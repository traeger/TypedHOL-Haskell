\begin{code}
module SZSStatus where

import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap

import Data.Maybe as Maybe

data SZSStatus 
  = SZSSuccess SZSSuccessReason 
  | SZSNoSuccess SZSNoSuccessReason deriving (Eq, Ord)

instance Show SZSStatus where
  show (SZSSuccess reason) = show reason
  show (SZSNoSuccess reason) = show reason

-- get the SZSStatus from a status name (like "Theorem", "CounterEquivalent", "Stopped"...)
fromName :: String -> Maybe SZSStatus
fromName reason = Maybe.listToMaybe $ Maybe.catMaybes $ 
  [ fmap SZSSuccess $ fromSuccessName reason
  , fmap SZSNoSuccess $ fromNoSuccessName reason
  ]

-- get the SZSStatus from a status type (like "THM", "ERR", "GUP"...)
fromType :: String -> Maybe SZSStatus
fromType reason = Maybe.listToMaybe $ Maybe.catMaybes $
  [ fmap SZSSuccess $ fromSuccessType reason
  , fmap SZSNoSuccess $ fromNoSuccessType reason
  ]

\end{code}
/**
 * SZS Success status values, as given by the following ontology diagram:
 * {{{
 *                                 Success
 *                                   SUC
 *         ___________________________|_______________________________
 *        |         |    |                                  |         |
 *     UnsatPre  SatPre  |                             CtrSatPre CtrUnsatPre
 *       UNP       SAP   |                                 CSP       CUP
 *        |_______/ |    |                                  | \_______|
 *        |         |    |                                  |         |
 *     EquSat       | FiniteThm                             |     EquCtrSat
 *       ESA        |   FTH                                 |        ECS
 *        |         |   /                                   |         |
 *     Sat'ble   Theorem                                 CtrThm     CtrSat
 *       SAT       THM                                     CTH       CSA
 *      / | \______.|._____________________________________.|.______/ | \
 *     /  |         |                   |                   |         |  \
 * FinSat |         |                NoConq                 |  FinUns | FinCtrSat
 *  FSA   |         |                  NOC                  |     FUN |   FCS
 *        |         |_______________________________________|       | |
 *        |         |                   |                   |       | |
 *        |     SatAxThm             CtraAx              SatAxCth   | |
 *        |        STH                 CAX                 SCT      : |
 *       _|_________|_              ____|____              _|_________|_
 *      |      |      |            |         |            |      |  :   |
 *   Eqvlnt  TautC  WeakC      SatConCA   SatCCoCA      WkCC  UnsCon|CtrEqu
 *     EQV    TAC    WEC          SCA       SCC          WCC    UNC |  CEQ
 *    __|__   _|_   __|__        __|___   ___|__        __|__   _|_ |__|__
 *   |     | /   \ |     |      |      \ /      |      |     | /   \|     |
 *Equiv  Taut-  Weaker Weaker TauCon   WCon  UnsCon Weaker Weaker Unsat Equiv
 * Thm   ology  TautCo  Thm   CtraAx  CtraAx CtraAx CtrThm UnsCon -able CtrTh
 * ETH    TAU    WTC    WTH    TCA     WCA     UCA    WCT    WUC    UNS  ECT
 * }}}
 * taken from [[https://github.com/leoprover/Leo-III/blob/master/src/main/scala/leo/modules/output/StatusSZS.scala]].
 * taken from [[http://www.cs.miami.edu/~tptp/cgi-bin/SeeTPTP?Category=Documents&File=SZSOntology]].
 */
\begin{code}
data SZSSuccessReason
  = SUC
-- layer 1
  | UNP
  | SAP
  | CSP
  | CUP
-- layer 2
  | ESA
  | FTH
  | ECS
-- layer 3
  | SAT
  | THM
  | CTH
  | CSA
-- layer 4
  | FSA
  | NOC
  | FUN
  | FCS
-- layer 5
  | STH
  | CAX
  | SCT
-- layer 6
  | EQV
  | TAC
  | WEC
  | SCA
  | SCC
  | WCC
  | UNC
  | CEQ
-- layer 7
  | ETH
  | TAU
  | WTC
  | WTH
  | TCA
  | WCA
  | UCA
  | WCT
  | WUC
  | UNS
  | ECT deriving (Eq, Ord)

successNames = 
  [ (SUC, "SUC", "Success")
-- layer 1
  , (UNP, "UNP", "UnsatisfiabilityPreserving")
  , (SAP, "SAP", "SatisfiabilityPreserving")
  , (CSP, "CSP", "CounterSatisfiabilityPreserving")
  , (CUP, "CUP", "CounterUnsatisfiabilityPreserving")
-- layer 2
  , (ESA, "ESA", "EquiSatisfiable")
  , (FTH, "FTH", "FiniteTheorem")
  , (ECS, "ECS", "EquiCounterSatisfiable")
-- layer 3
  , (SAT, "SAT", "Satisfiable")
  , (THM, "THM", "Theorem")
  , (CTH, "CTH", "CounterTheorem")
  , (CSA, "CSA", "CounterSatisfiable")
-- layer 4
  , (FSA, "FSA", "FinitelySatisfiable")
  , (NOC, "NOC", "NoConsequence")
  , (FUN, "FUN", "FinitelyUnsatisfiable")
  , (FCS, "FCS", "FinitelyCounterSatisfiable")
-- layer 5
  , (STH, "STH", "SatisfiableTheorem")
  , (CAX, "CAX", "ContradictoryAxioms")
  , (SCT, "SCT", "SatisfiableCounterTheorem")
-- layer 6
  , (EQV, "EQV", "Equivalent")
  , (TAC, "TAC", "TautologousConclusion")
  , (WEC, "WEC", "WeakerConclusion")
  , (SCA, "SCA", "SatisfiableConclusionContradictoryAxioms")
  , (SCC, "SCC", "SatisfiableCounterConclusionContradictoryAxioms")
  , (WCC, "WCC", "WeakerCounterConclusion")
  , (UNC, "UNC", "UnsatisfiableConclusion")
  , (CEQ, "CEQ", "CounterEquivalent")
-- layer 7
  , (ETH, "ETH", "EquivalentTheorem")
  , (TAU, "TAU", "Tautology")
  , (WTC, "WTC", "WeakerTautologousConclusion")
  , (WTH, "WTH", "WeakerTheorem")
  , (TCA, "TCA", "TautologousConclusionContradictoryAxioms")
  , (WCA, "WCA", "WeakerConclusionContradictoryAxioms")
  , (UCA, "UCA", "UnsatisfiableConclusionContradictoryAxioms")
  , (WCT, "WCT", "WeakerCounterTheorem")
  , (WUC, "WUC", "WeakerUnsatisfiableConclusion")
  , (UNS, "UNS", "Unsatisfiable")
  , (ECT, "ECT", "EquivalentCounterTheorem")
  ]

bimapSZSSuccessType :: Bimap SZSSuccessReason String
bimapSZSSuccessType = Bimap.fromList $ map (\(c,t,_) -> (c,t)) $ successNames
bimapSZSSuccessName :: Bimap SZSSuccessReason String
bimapSZSSuccessName = Bimap.fromList $ map (\(c,_,n) -> (c,n)) $ successNames

toSuccessName reason = bimapSZSSuccessName Bimap.! reason
toSuccessType reason = bimapSZSSuccessType Bimap.! reason
fromSuccessName reason = Bimap.lookupR reason bimapSZSSuccessName
fromSuccessType reason = Bimap.lookupR reason bimapSZSSuccessName

instance Show SZSSuccessReason where
  show = toSuccessName
\end{code}

/**
  * Unsuccessful result status, as given by the NoSuccess ontology:
  * {{{
  *                                            NoSuccess
  *                                               NOS
  *                            ____________________|___________________
  *                           |                    |                   |
  *                         Open                Unknown             Assumed
  *                          OPN                  UNK             ASS(UNK,SUC)
  *                               _________________|_________________
  *                              |                 |                 |
  *                           Stopped         InProgress         NotTried
  *                             STP               INP               NTT
  *          ____________________|________________               ____|____
  *         |                    |                |             |         |
  *       Error               Forced           GaveUp           |    NotTriedYet
  *        ERR                  FOR              GUP            |        NTY
  *     ____|____            ____|____   _________|__________   |
  *    |         |          |         | |         |     |    |  |
  * OSError   InputEr      User   ResourceOut  Incompl  |  Inappro
  *   OSE       INE        USR        RSO        INC    |    IAP
  *           ___|___              ___|___             v
  *          |   |   |            |       |           to
  *      UseEr SynEr SemEr    Timeout MemyOut        ERR
  *         USE SYE SEE         TMO     MMO
  *                  |
  *              TypeError
  *                 TYE
  * }}}
  *
  * taken from [[https://github.com/leoprover/Leo-III/blob/master/src/main/scala/leo/modules/output/StatusSZS.scala]].
  * taken from [[http://www.cs.miami.edu/~tptp/cgi-bin/SeeTPTP?Category=Documents&File=SZSOntology]].
  */
\begin{code}
data SZSNoSuccessReason
  = NOS
-- layer 1
  | OPN
  | UNK
  | ASS
-- layer 2
  | STP
  | INP
  | NTT
-- layer 3
  | ERR
  | FOR
  | GUP
  | NTY
-- layer 4
  | OSE
  | INE
  | USR
  | RSO
  | INC
  | IAP
-- layer 5
  | USE
  | SYE
  | SEE
  | TMO
  | MMO
-- layer 6
  | TYE deriving (Eq, Ord)

nosuccessNames = 
  [ (NOS, "NOS", "NoSuccess")
-- layer 1
  , (OPN, "OPN", "Open")
  , (UNK, "UNK", "Unknown")
  , (ASS, "ASS", "Assumed")
-- layer 2
  , (STP, "STP", "Stopped")
  , (INP, "INP", "InProgress")
  , (NTT, "NTT", "NotTried")
-- layer 3
  , (ERR, "ERR", "Error")
  , (FOR, "FOR", "Forced")
  , (GUP, "GUP", "GaveUp")
  , (NTY, "NTY", "NotTriedYet")
-- layer 4
  , (OSE, "OSE", "OSError")
  , (INE, "INE", "InputError")
  , (USR, "USR", "User")
  , (RSO, "RSO", "ResourceOut")
  , (INC, "INC", "Incomplete")
  , (IAP, "IAP", "Inappropriate")
-- layer 5
  , (USE, "USE", "UsageError")
  , (SYE, "SYE", "SyntaxError")
  , (SEE, "SEE", "SemanticError")
  , (TMO, "TMO", "Timeout")
  , (MMO, "MMO", "MemoryOut")
-- layer 6
  , (TYE, "TYE", "TypeError")
  ]

bimapSZSNoSuccessType :: Bimap SZSNoSuccessReason String
bimapSZSNoSuccessType = Bimap.fromList $ map (\(c,t,_) -> (c,t)) $ nosuccessNames
bimapSZSNoSuccessName :: Bimap SZSNoSuccessReason String
bimapSZSNoSuccessName = Bimap.fromList $ map (\(c,_,n) -> (c,n)) $ nosuccessNames

toNoSuccessName reason = bimapSZSNoSuccessName Bimap.! reason
toNoSuccessType reason = bimapSZSNoSuccessType Bimap.! reason
fromNoSuccessName reason = Bimap.lookupR reason bimapSZSNoSuccessName
fromNoSuccessType reason = Bimap.lookupR reason bimapSZSNoSuccessName

instance Show SZSNoSuccessReason where
  show = toNoSuccessName
\end{code}