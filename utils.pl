%Copyright [2020] [Laura Bussi, Alessandro Di Giorgio, Selman Alpdundar, Tabriz Hajiev]

%   Licensed under the Apache License, Version 2.0 (the "License");
%   you may not use this file except in compliance with the License.
%   You may obtain a copy of the License at

%       http://www.apache.org/licenses/LICENSE-2.0

%   Unless required by applicable law or agreed to in writing, software
%   distributed under the License is distributed on an "AS IS" BASIS,
%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%   See the License for the specific language governing permissions and
%   limitations under the License.

:- use_module(library(lists)).
:- use_module(library(assert)).

% ACCESS CONTROL

canRead(Res, CD, COI, []).
canRead(Res, CD, COI, [(O, OCD, OCOI)|Rest]) :- 
    readCondition(O, OCD, OCOI, Res, CD, COI), 
    canRead(Res, CD, COI, Rest).

readCondition(O, OCD, OCOI, Res, CD, COI) :- 
    O \== Res, (OCOI \== COI; OCD == CD).

canWrite(Res, CD, COI, []).
canWrite(Res, CD, COI, [(O, OCD, OCOI)|Rest]) :- 
    writeCondition(O, OCD, OCOI, Res, CD, COI), 
    canWrite(Res, CD, COI, Rest).

writeCondition(O, OCD, OCOI, Res, CD, COI) :-
    readCondition(O, OCD, OCOI, Res, CD, COI), OCD == CD.

findNode(Dest, Node, [(Dest, Node) | Rest]).
findNode(Dest, Node, [(Service, Node2) | Rest]) :- 
    Service \== Dest,
    findNode(Dest, Node, Rest).

findRoute(_, Latency, Latency, _, Source, Source, _, []).
findRoute(AOp, OldLatency, Latency, Old, Source, Dest, ReqSecurity, [Source | Route]) :- 
    Source \== Dest,
    isConnected(Source, Step, LId, LinkLatency),
    Old \== Step,
    OldLatency =< LinkLatency,
    labelL(AOp, LId, ReqSecurity),
    findRoute(AOp, LinkLatency, Latency, Source, Step, Dest, ReqSecurity, Route).

findRoute(AOp, OldLatency, Latency, Old, Source, Dest, ReqSecurity, [Source | Route]) :- 
    Source \== Dest,
    isConnected(Source, Step, LId, LinkLatency),
    Old \== Step,
    LinkLatency =< OldLatency,
    labelL(AOp, LId, ReqSecurity),
    findRoute(AOp, OldLatency, Latency, Source, Step, Dest, ReqSecurity, Route).

isConnected(Source, Step, L, LinkLatency) :-
    link(L, LinkLatency, [Source, Step]); link(L, LinkLatency, [Step, Source]). 

computeCost(Placement, Cost) :- computeCost(Placement, 0, Cost).
computeCost([],Cost,Cost).
computeCost([(SId, FId, NId)|Placement], Cost, NewCost) :-
    func(FId, _, _, _, TUnits),
    node(NId, _, _, _, _, CostPU, _),
    TmpCost is TUnits * CostPU + Cost,
    computeCost(Placement, TmpCost, NewCost).

% checks if require
checkPlatforms(PReqs, Plats) :- member(PReqs, Plats).

checkContext(AOp, Args, NId, OpN, Geo, L) :- 
    labelN(AOp, NId, OpN, Geo, L), 
    labelF(AOp, Args, L).

checkHw(HwCaps, HwReqs, NId, [], [(NId, NewFree)]):- NewFree is HwCaps - HwReqs.
checkHw(HwCaps, HwReqs, NId, [(NId2, R) | Rest], [(NId2, R) | NewFree]) :- 
NId \== NId2,
    checkHw(HwCaps, HwReqs, NId, Rest, NewFree).
checkHw(_, HwReqs, NId, [(NId, Free) | Rest], [(NId, NewFree)|Rest]) :-
HwReqs =< Free,
NewFree is Free - HwReqs.

% lattice ordering
leq(AOp, X, X).               
leq2(AOp, A, B) :- leq(AOp, A, B).   
leq2(AOp, A, B) :- leq(AOp, A, C), leq2(AOp, C, B), A \== B.

labelL(AOp, L, Lbl) :- 
                    link(L, _, [N1, N2]),
                    node(N1, OpN, _, _, _, _, Geo),
                    node(N2, OpN, _, _, _, _, Geo),
                    labelN(AOp, N1, OpN, Geo, Lbl),
                    labelN(AOp, N2, OpN, Geo, Lbl).

% labels a node with its security context
labelN(default, N, OpN, Geo, ts) :- 
                            member(Geo, [eu,ch]), 
                            firewall(N), 
                            member(OpN, [amazon, azure]).
labelN(default, N, OpN, Geo, s) :- 
                            member(Geo, [eu,ch,us]).
labelN(default, N, OpN, Geo, l) :- 
                            member(Geo, [eu,ch,us,vat]).


% checks if node label supports function label
supports(AOp,NId,l) :- labelN(AOp,NId,_).
supports(AOp,NId,s) :- labelN(AOp,NId,L), L \== l.
supports(AOp,NId,ts) :- labelN(AOp,NId,ts). 

% default trust model à la SecFog
trusts(X,X).               
trusts2(A,B) :- trusts(A,B).
trusts2(A,B) :- trusts(A,C),trusts2(C,B), A \== B.

checkTime(tau, 0).
checkTime(read(_, _, _, _), 0).
checkTime(write(_, _, _, _), 0).
checkTime(send(_, _, _), 0).
checkTime(fireTrigger(_, _, _), 0).
checkTime(par(P1, P2), SeqTUnits) :- 
    checkTime(P1, FstTUnits), 
    checkTime(P2, SndTUnits), 
    SeqTUnits is max(FstTUnits, SndTUnits).
checkTime(seq(P1, P2), SeqTUnits) :- 
    checkTime(P1, FstTUnits), 
    checkTime(P2, SndTUnits), 
    SeqTUnits is FstTUnits + SndTUnits.
checkTime(FId, TUnits) :- func(FId, _, _, _, TUnits).
checkTime(whl(FId, P), TUnitsW) :- 
    checkTime(FId, CTUnits), 
    checkTime(P, BTunits), 
    TUnitsW is CTUnits + BTunits.
checkTime(trc(P1, P2), TUnitsTRC) :- 
    checkTime(P1, TTUnits), 
    checkTime(P2, CTUnits), 
    TTUnits == CTUnits, 
    TUnitsTRC is TTUnits + CTUnits.
checkTime(ife(FId, P1, P2), TUnits) :- 
    checkTime(FId, CG), 
    checkTime(P1, TUnitsT), 
    checkTime(P2, TUnitsE), 
    TUnitsT == TUnitsE,
    TUnits is TUnitsT.
