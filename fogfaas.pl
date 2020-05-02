%%%%%%%%% Working (problog) code %%%%%%%%%

:- use_module(library(lists)).

%placeServices(AOp, [], Placement, NewPlacement, _, _) :- append(Placement, [], NewPlacement).
%
%placeServices(AOp, [SId|Rest], Placement, [NewPlacement, Caps, NewCaps) :-
%    service(SId, _, Prog, HwReqs, PReqs, Geo),
%    node(NId, OpN, HwCaps, Plats, CostPU, NodeLoc),
%    member(NodeLoc, Geo),
%    member(PReqs, Plats),
%    trusts2(AOp, OpN),
%    checkHw(HwCaps, HwReqs, NId, Caps, TmpCaps),
%    placeFunctionsAux(AOp, Prog, Placement, FunPlacement, TmpCaps, FunCaps),
%    placeServices(AOp, Rest, [(SId, NId)|FunPlacement], NewPlacement, TmpCaps, NewCaps).

:- use_module(library(lists)).

%app(AId, [SIds]).
app(app1, [service1, service2]).

placeServices(AOp, [], P, P, C, C).
placeServices(AOp, [SId|Rest], Placement, [(SId, NId)|NewPlacement], Caps, NewCaps) :-
    service(SId, _, Prog, HwReqs, PReqs, Geo),
    node(NId, OpN, HwCaps, Plats, CostPU, NodeLoc),
    member(NodeLoc, Geo),
    member(PReqs, Plats),
    trusts2(AOp, OpN),
    checkHw(HwCaps, HwReqs, NId, Caps, TmpCaps),
    placeServices(AOp, Rest, Placement, NewPlacement, TmpCaps, NewCaps).

placeApp(AOp, AId, ServicePlacement, FunctionPlacement):-
    app(AId, Services),
    placeServices(AOp, Services, [], ServicePlacement, [], Caps).
    placeAllFunctions(AOp, ServicePlacement, [], FunctionPlacement, Caps).

placeAllFunctions(_, [], FP, FP, _).
placeAllFunctions(AOp, [(SId, _)|Placement], FPlacement, NewFPlacement, Caps) :-
    service(SId, _, Prog, _, _, _),
    placeFunctions(AOp, SId, Prog, FPlacement, TmpFPlacement, Caps, NewCaps),
    placeAllFunctions(AOp, Placement, TmpFPlacement, NewFPlacement, Caps).
    

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
      node(NId, OpN, HwCaps, Plats, CostPU, Geo),
      trusts2(AOp, OpN),
      checkPlatforms(PReqs, Plats),
      checkContext(AOp, Args, NId, OpN, Geo, L),
      HwReqs =< HwCaps, checkHw(HwCaps, HwReqs, NId, Caps, NewCaps).   

placeFunctions(AOp, SId, seq(P1, P2), Placement, NewPlacement, Caps, NewCaps) :-
      placeFunctions(AOp, SId, P1, Placement, PlacementTmp1, Caps, NewCaps),
      placeFunctions(AOp, SId, P2, Placement, PlacementTmp2, Caps, NewCaps),
      append(PlacementTmp1, PlacementTmp2, NewPlacement).

placeFunctions(AOp, SId, FId, Placement, [(SId, FId, NId)|Placement], Caps, NewCaps) :-
      func(FId, Args, HwReqs, PReqs, TUnits),
      node(NId, OpN, HwCaps, Plats, CostPU, Geo),
      trusts2(AOp, OpN),
      checkPlatforms(PReqs, Plats),
      checkContext(AOp, Args, NId, OpN, Geo, L),
      HwReqs =< HwCaps, checkHw(HwCaps, 0, NId, Caps, NewCaps).  

placeFunctions(AOp, SId, ife(FId, P1, P2), Placement, [(SId, FId, NId)|NewPlacement], Caps, NewCaps) :-
   func(FId, Args, HwReqs, PReqs, TUnits),
   node(NId, OpN, HwCaps, Plats, CostPU, Geo),
   trusts2(AOp, OpN),
   checkPlatforms(PReqs, Plats),
   checkContext(AOp, Args, NId, OpN, Geo, L),
   HwReqs =< HwCaps,
   placeFunctions(AOp, SId, P1, Placement, NewPlacement, Caps, NewCaps).

placeFunctions(AOp, SId, ife(FId, P1, P2), Placement, [(SId, FId, NId)|NewPlacement], Caps, NewCaps) :-
   func(FId, Args, HwReqs, PReqs, TUnits),
   node(NId, OpN, HwCaps, Plats, CostPU, Geo),
   trusts2(AOp, OpN),
   checkPlatforms(PReqs, Plats),
   checkContext(AOp, Args, NId, OpN, Geo, L),
   HwReqs =< HwCaps,
   placeFunctions(AOp, SId, P2, Placement, NewPlacement, Caps, NewCaps).

placeFunctions(AOp, SId, whl(FId, P), Placement, [(SId, FId, NId)|NewPlacement], Caps, NewCaps) :-
    func(FId, Args, HwReqs, PReqs, TUnits),
    node(NId, OpN, HwCaps, Plats, CostPU, Geo),
    trusts2(AOp, OpN),
    checkPlatforms(PReqs, Plats),
    checkContext(AOp, Args, NId, OpN, Geo, L),
    HwReqs =< HwCaps,
    placeFunctions(AOp, SId, P, Placement, NewPlacement, Caps, NewCaps).

placeFunctions(AOp, SId, trc(P1, P2), Placement, NewPlacement, Caps, NewCaps) :-
      placeFunctions(AOp, SId, P1, Placement, PlacementTmp1, Caps, NewCaps),
      placeFunctions(AOp, SId, P2, Placement, PlacementTmp2, Caps, NewCaps),
      append(PlacementTmp1, PlacementTmp2, NewPlacement).

computeCost(Placement, Cost) :- computeCost(Placement, 0, Cost).
computeCost([],Cost,Cost).
computeCost([(FId, NId)|Placement], Cost, NewCost) :-
    func(FId, _, _, _, TUnits),
    node(NId, _, _, _, CostPU, _),
    TmpCost is TUnits * CostPU + Cost,
    computeCost(Placement, TmpCost, NewCost).

% checks if require
checkPlatforms(PReqs, Plats) :- member(PReqs, Plats).

checkContext(AOp, Args, NId, OpN, Geo, L) :- labelN(AOp, NId, OpN, Geo, L), 
   labelF(AOp, Args, L).


% labels a node with its security context
labelN(ann, N, OpN, Geo, ts) :- 
member(Geo, [eu,ch]), 
firewall(N), 
member(OpN, [amazon, azure]).

labelN(ann, N, OpN, Geo, s) :- member(Geo, [eu,ch,us]).

labelN(ann, N, OpN, Geo, l) :- member(Geo, [eu,ch,us,vat]).

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

%security context
ctx(_, tau, _).
ctx(AOp, seq(P1, P2), L) :- ctx(AOp, P1, L), ctx(AOp, P2, L).
ctx(AOp, ife(FId, P1, P2), L) :-
   func(FId, Args, _, _, _),
   labelF(AOp, Args, L),
   ctx(AOp, P1, L),
   ctx(AOp, P2, L).
ctx(AOp, whl(FId, P), L) :-
   func(FId, Args, _, _, _),
   labelF(AOp, Args, L),
   ctx(AOp, P, L).
ctx(AOp, trc(P1, P2), L) :- ctx(AOp, P1, L), ctx(AOp, P2, L).
ctx(AOp, FId, L) :- func(FId, Args, _, _, _), labelF(AOp, Args, L).

%%%%%%%%% Working (problog) code %%%%%%%%%


ts(z).
s(x).
l(y).
l(t).

% functions

func(sum, [x,y], 1, rust, 10).
func(mult,[y,t], 1, java, 10).
func(div, [z,z], 2, python, 20).

%service(SId, Trigger, Program, HWReqs, PReqs, GeoReqList, TimeUnits).
service(service1, triggerX, sum, 1, python, [eu]).
service(service2, triggerY, div, 1, java, [eu]).

node(n1, amazon, 4, [python, rust, java, javascript], 0.001, eu).
encrypted_storage(n1).
firewall(n1).

node(n2, amazon, 5, [python, rust, java, javascript], 0.001, eu).
encrypted_storage(n2).
firewall(n2).

trusts(ann, amazon).

%app(OpA, AId, [SIds]).
app(app1, [service1, service2]).

%query(placeFunctions(ann, service1, seq(mult, div), [], R, [], C)).

query(placeApp(ann, app1, SP, FP)).
