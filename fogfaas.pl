%%%%%%%%% Working (problog) code %%%%%%%%%

:- consult('app.pl').
:- consult('infra.pl').
:- use_module(library(lists)).

%app(AId, [SIds]).
app(app1, [service1, service2]).

placeServices(AOp, [], P, P, C, C).
placeServices(AOp, [SId|Rest], Placement, [(SId, NId)|NewPlacement], Caps, NewCaps) :-
    service(SId, _, Prog, HwReqs, PReqs, Geo),
    node(NId, OpN, HwCaps, SPlats, _, CostPU, NodeLoc),
    member(NodeLoc, Geo),
    subset(PReqs, SPlats),
    trusts2(AOp, OpN),
    checkHw(HwCaps, HwReqs, NId, Caps, TmpCaps),
    placeServices(AOp, Rest, Placement, NewPlacement, TmpCaps, NewCaps).

placeApp(AOp, AId, ServicePlacement, FunctionPlacement):-
    app(AId, Services),
    placeServices(AOp, Services, [], ServicePlacement, [], Caps),
    placeAllFunctions(AOp, ServicePlacement, [], FunctionPlacement, Caps).

placeAllFunctions(_, [], FP, FP, _).
placeAllFunctions(AOp, [(SId, _)|Placement], FPlacement, NewFPlacement, Caps) :-
    service(SId, _, Prog, _, _, _),
    placeFunctions(AOp, SId, Prog, FPlacement, TmpFPlacement, Caps, NewCaps),
    placeAllFunctions(AOp, Placement, TmpFPlacement, NewFPlacement, NewCaps).
    

% places each function onto a target node, following the orchestrator service code
%placeFunctions(AOp, Prog, Placement, Cost) :-
%      placeFunctionsAux(AOp, Prog, [], Placement, [], NewCaps),
%      computeCost(Placement, Cost).

placeFunctions(_, _, tau, _, _, [], _).

placeFunctions(AOp, SId, par(F1, F2), Placement, NewPlacement, Caps, NewCaps) :-
      placeParFunctions(AOp, SId, F1, Placement, PlacementTmp, Caps, CapsTmp),
      placeParFunctions(AOp, SId, F2, PlacementTmp, NewPlacement, CapsTmp, NewCaps).

placeParFunctions(AOp, SId, FId, Placement, [(SId, FId, NId)|Placement], Caps, NewCaps) :-
      func(FId, Args, HwReqs, PReqs, TUnits),
      node(NId, OpN, HwCaps, SPlats, FPlats, CostPU, Geo),
      trusts2(AOp, OpN),
      checkPlatforms(PReqs, FPlats),
      checkContext(AOp, Args, NId, OpN, Geo, L),
      HwReqs =< HwCaps, checkHw(HwCaps, HwReqs, NId, Caps, NewCaps).   

placeFunctions(AOp, SId, seq(P1, P2), Placement, NewPlacement, Caps, NewCaps) :-
      placeFunctions(AOp, SId, P1, Placement, PlacementTmp1, Caps, NewCaps1),
      placeFunctions(AOp, SId, P2, Placement, PlacementTmp2, Caps, NewCaps2),
      append(PlacementTmp1, PlacementTmp2, NewPlacement).

placeFunctions(AOp, SId, FId, Placement, [(SId, FId, NId)|Placement], Caps, NewCaps) :-
    func(FId, Args, HwReqs, PReqs, TUnits),
    node(NId, OpN, HwCaps, SPlats, FPlats, CostPU, Geo),
    trusts2(AOp, OpN),
    checkPlatforms(PReqs, FPlats),
    checkContext(AOp, Args, NId, OpN, Geo, L),
    HwReqs =< HwCaps, checkHw(HwCaps, HwReqs, NId, Caps, NewCaps).  

placeFunctions(AOp, SId, ife(FId, P1, P2), Placement, [(SId, FId, NId)|NewPlacement], Caps, NewCaps) :-
   func(FId, Args, HwReqs, PReqs, TUnits),
   node(NId, OpN, HwCaps, SPlats, FPlats, CostPU, Geo),
   trusts2(AOp, OpN),
   checkPlatforms(PReqs, FPlats),
   checkContext(AOp, Args, NId, OpN, Geo, L),
   HwReqs =< HwCaps,
   placeFunctions(AOp, SId, P1, Placement, NewPlacement, Caps, NewCaps).

placeFunctions(AOp, SId, ife(FId, P1, P2), Placement, [(SId, FId, NId)|NewPlacement], Caps, NewCaps) :-
   func(FId, Args, HwReqs, PReqs, TUnits),
   node(NId, OpN, HwCaps, SPlats, FPlats, CostPU, Geo),
   trusts2(AOp, OpN),
   checkPlatforms(PReqs, FPlats),
   checkContext(AOp, Args, NId, OpN, Geo, L),
   HwReqs =< HwCaps,
   placeFunctions(AOp, SId, P2, Placement, NewPlacement, Caps, NewCaps).

placeFunctions(AOp, SId, whl(FId, P), Placement, [(SId, FId, NId)|NewPlacement], Caps, NewCaps) :-
    func(FId, Args, HwReqs, PReqs, TUnits),
    node(NId, OpN, HwCaps, SPlats, FPlats, CostPU, Geo),
    trusts2(AOp, OpN),
    checkPlatforms(PReqs, FPlats),
    checkContext(AOp, Args, NId, OpN, Geo, L),
    HwReqs =< HwCaps,
    placeFunctions(AOp, SId, P, Placement, NewPlacement, Caps, NewCaps).

placeFunctions(AOp, SId, trc(P1, P2), Placement, NewPlacement, Caps, NewCaps) :-
      placeFunctions(AOp, SId, P1, Placement, PlacementTmp1, Caps, NewCaps1),
      placeFunctions(AOp, SId, P2, Placement, PlacementTmp2, Caps, NewCaps2),
      append(PlacementTmp1, PlacementTmp2, NewPlacement).

computeCost(Placement, Cost) :- computeCost(Placement, 0, Cost).
computeCost([],Cost,Cost).
computeCost([(FId, NId)|Placement], Cost, NewCost) :-
    func(FId, _, _, _, TUnits),
    node(NId, _, _, _, _, CostPU, _),
    TmpCost is TUnits * CostPU + Cost,
    computeCost(Placement, TmpCost, NewCost).

% checks if require
checkPlatforms(PReqs, Plats) :- member(PReqs, Plats).

checkContext(AOp, Args, NId, OpN, Geo, L) :- labelN(AOp, NId, OpN, Geo, L), 
   labelF(AOp, Args, L).



checkHw(HwCaps, HwReqs, NId, [], [(NId, NewFree)]):- NewFree is HwCaps - HwReqs.
checkHw(HwCaps, HwReqs, NId, [(NId2, R) | Rest], [(NId2, R) | NewFree]) :- 
NId \== NId2,
    checkHw(HwCaps, HwReqs, NId, Rest, NewFree).
checkHw(_, HwReqs, NId, [(NId, Free) | Rest], [(NId, NewFree)|Rest]) :-
HwReqs =< Free,
NewFree is Free - HwReqs.

labelF(ann, Args, ts).
labelF(ann, Args, s) :- findall(X, ts(X, Args), []).
labelF(ann, Args, l) :- findall(X, notPublic(X, Args), []).
notPublic(X, Args) :-  member(X, Args),(ts(X);s(X)). 
ts(X, Args) :- member(X, Args), ts(X).


% labels a service composing multiple functions
labelS(AOp, SId, L) :- service(SId, _, P, _, _, _), ctx(AOp, P, L).

% checks if node label supports function label
supports(AOp,NId,l) :- labelN(AOp,NId,_).
supports(AOp,NId,s) :- labelN(AOp,NId,L), L \== l.
supports(AOp,NId,ts) :- labelN(AOp,NId,ts). 

% default trust model à la SecFog
trusts(X,X).               
trusts2(A,B) :- trusts(A,B).   
trusts2(A,B) :- trusts(A,C),trusts2(C,B), A \== B.

% security context
ctx(_, tau, _, _).

ctx(AOp, seq(P1, P2), SeqTUnits,L) :- 
                ctx(AOp, P1, FstTUnits, L), % FstUnits  = time units of first program
                ctx(AOp, P2, SndTUnits, L), % SndTUnits = time units of second program.
                SeqTUnits is FstTUnits + SndTUnits. % SeqTunits = time unit of sequential programs.

ctx(AOp, ife(FId, P1, P2), TUnits, L) :-
                func(FId, Args, _, _, _), % ?
                labelF(AOp, Args, L),
                ctx(AOp, P1,TUnitsT, L), % TUnitsT = time units of then
                ctx(AOp, P2,TUnitsE, L), % TUnitsE = time units of else
                TUnitsT == TUnitsE,      % TUnitsT must be equal to TUnitsT
                TUnits is TUnitsT.       % TUnits = time units of if then else

ctx(AOp, whl(FId, P), TUnitsW, L) :-
                func(FId, Args, _, _, CTUnits), % CTunits = time unit of guard
                labelF(AOp, Args, L),
                guardCheck(L),
                ctx(AOp, P, BTunits, L),    % BTunits = time unit of body of while.
                TUnitsW is CTUnits + BTunits.  % TUnitsW = time unite of while
                

ctx(AOp, trc(P1, P2), TUnitsTRC ,L) :- 
                ctx(AOp, P1, TTUnits, L),  %  TTUnits = time units of try
                ctx(AOp, P2, CTUnits, L),  %  CTUnits = time units of catch
                TTUnits == CTUnits,        %  The logic same with if-then-else
                TUnitsTRC is TTUnits.      %  TUnitsTRC = time units of try catch

ctx(AOp, FId, TUnits,L) :- 
                func(FId, Args, _, _, TUnits), % TUnits = Computational time for FId
                labelF(AOp, Args, L). 

% those are the Label that are not allowed to use in guard
guardCheck(X):- not(X = ts),    
                not(X = s),
                not(X = ts_eu),
                not(X = ts_us),
                not(X = s_eu),
                not(X = s_us).

%       ts
%       |
%       s
%       |
%       l

%   ts_eu   ts_us
%     |       |
%   s_eu    s_us
%      \    /
%        l

% l is the public label for two latices there fore
% this guard will work for both latices

%query(placeFunctions(ann, service1, seq(mult, div), [], R, [], C)).
%query(placeApp(ann, app1, SP, FP)).

% Security Context Test
ts(z).
s(x).
l(y).
l(t).

func(sum, [x,y], 1, rust, 10).
func(mult,[y,t], 1, java, 10).
func(div, [z,z], 2, python, 20).

% --------- if-then-else-test ----------
% if else with two program
% secure 
query(ctx(ann,ife(div, mult, sum), _,L)).
% insecure
query(ctx(ann,ife(div, mult, div), _,L)).

% if with sequential program
% secure         
query(ctx(ann,ife(div, seq(sum,sum), div ), _,L)).
query(ctx(ann,ife(mult, div, seq(sum,sum)), _,L)).
query(ctx(ann,ife(mult, seq(mult,mult), seq(sum,sum)), _,L)).

% insecure
query(ctx(ann,ife(mult, seq(mult,mult),sum), _,L)).
query(ctx(ann,ife(mult,sum,seq(mult,mult)), _,L)).

% if with while loop
%secure
query(ctx(ann,ife(mult, whl(sum,sum),seq(sum,sum)), _,L)).
query(ctx(ann,ife(mult, whl(sum,sum),div), _,L)).
%insecure
query(ctx(ann,ife(mult, whl(sum,sum),sum), _,L)).

% if with trc 
% secure
query(ctx(ann,ife(mult,trc(sum,sum),sum),T,L)).
query(ctx(ann,ife(mult,trc(div,div),seq(sum,sum)),T,L)).

% insecure
query(ctx(ann,ife(mult,trc(div,sum),sum),T,L)).
query(ctx(ann,ife(mult,trc(div,sum),seq(sum,sum)),T,L)).


% While test
% secure
query(ctx(ann,whl(mult,mult),T,L)).
% insecure
query(ctx(ann,whl(div,sum),T,L)).
query(ctx(ann,whl(sum,sum),T,L)).