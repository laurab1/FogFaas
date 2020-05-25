%%%%%%%%% Working (problog) code %%%%%%%%%

:- use_module(library(lists)).
:- use_module(library(assert)).

placeServices(AOp, [], P, P, C, C).
placeServices(AOp, [SId|Rest], Placement, [(SId, NId)|NewPlacement], Caps, NewCaps) :-
    service(SId, _, Prog, HwReqs, PReqs, Geo),
    ctx(AOp, Prog, L, [], Env, [], History),
    node(NId, OpN, HwCaps, SPlats, _, CostPU, NodeLoc),
    member(NodeLoc, Geo),
    subset(PReqs, SPlats),
    trusts2(AOp, OpN),
    checkHw(HwCaps, HwReqs, NId, Caps, TmpCaps),
    placeServices(AOp, Rest, Placement, NewPlacement, TmpCaps, NewCaps).

placeApp(AOp, AId, ServicePlacement, FunctionPlacement):-
    app(AId, Services),
    placeServices(AOp, Services, [], ServicePlacement, [], Caps).
    placeAllFunctions(AOp, ServicePlacement, ServicePlacement, [], FunctionPlacement, Caps).

placeAllFunctions(_, [], _, FP, FP, _).
placeAllFunctions(AOp, [(SId, Node)|Placement], GlobPlacement, FPlacement, NewFPlacement, Caps) :-
    service(SId, _, Prog, _, _, _),
    placeFunctions(AOp, (SId, Node), GlobPlacement, Prog, FPlacement, TmpFPlacement, Caps, NewCaps),
    placeAllFunctions(AOp, Placement, GlobPlacement, TmpFPlacement, NewFPlacement, NewCaps).
    
% places each function onto a target node, following the orchestrator service code
%placeFunctions(AOp, Prog, Placement, Cost) :-
%      placeFunctionsAux(AOp, Prog, [], Placement, [], NewCaps),
%      computeCost(Placement, Cost).

placeFunctions(_, _, _, tau, _, [], _, _).

placeFunctions(AOp, (SId, Node), ServicePlacement, par(F1, F2), Placement, NewPlacement, Caps, NewCaps) :-
      placeParFunctions(AOp, (SId, Node), _, F1, Placement, PlacementTmp, Caps, CapsTmp),
      placeParFunctions(AOp, (SId, Node), _, F2, PlacementTmp, NewPlacement, CapsTmp, NewCaps).

placeParFunctions(AOp, (SId, Node), ServicePlacement, FId, Placement, [(SId, FId, NId)|Placement], Caps, NewCaps) :-
      func(FId, Args, HwReqs, PReqs, TUnits),
      node(NId, OpN, HwCaps, SPlats, FPlats, CostPU, Geo),
      trusts2(AOp, OpN),
      checkPlatforms(PReqs, FPlats),
      checkContext(AOp, Args, NId, OpN, Geo, L),
      HwReqs =< HwCaps, checkHw(HwCaps, HwReqs, NId, Caps, NewCaps).   

placeFunctions(AOp, (SId, Node), ServicePlacement, seq(P1, P2), Placement, NewPlacement, Caps, NewCaps) :-
      placeFunctions(AOp, (SId, Node), ServicePlacement, P1, Placement, PlacementTmp1, Caps, NewCaps1),
      placeFunctions(AOp, (SId, Node), ServicePlacement, P2, Placement, PlacementTmp2, Caps, NewCaps2),
      append(PlacementTmp1, PlacementTmp2, NewPlacement).

placeFunctions(AOp, (SId, Node), ServicePlacement, FId, Placement, [(SId, FId, NId)|Placement], Caps, Caps) :-
    func(FId, Args, HwReqs, PReqs, TUnits),
    node(NId, OpN, HwCaps, SPlats, FPlats, CostPU, Geo),
    trusts2(AOp, OpN),
    checkPlatforms(PReqs, FPlats),
    checkContext(AOp, Args, NId, OpN, Geo, L),
    HwReqs =< HwCaps, checkHw(HwCaps, HwReqs, NId, Caps, NewCaps).

placeFunctions(AOp, (SId, Node), ServicePlacement, ife(FId, P1, P2), Placement, [(SId, FId, NId)|NewPlacement], Caps, NewCaps) :-
   func(FId, Args, HwReqs, PReqs, TUnits),
   node(NId, OpN, HwCaps, SPlats, FPlats, CostPU, Geo),
   trusts2(AOp, OpN),
   checkPlatforms(PReqs, FPlats),
   checkContext(AOp, Args, NId, OpN, Geo, L),
   HwReqs =< HwCaps,
   placeFunctions(AOp, (SId, Node), ServicePlacement, P1, Placement, NewPlacement, Caps, NewCaps).

placeFunctions(AOp, (SId, Node), ServicePlacement, ife(FId, P1, P2), Placement, [(SId, FId, NId)|NewPlacement], Caps, NewCaps) :-
   func(FId, Args, HwReqs, PReqs, TUnits),
   node(NId, OpN, HwCaps, SPlats, FPlats, CostPU, Geo),
   trusts2(AOp, OpN),
   checkPlatforms(PReqs, FPlats),
   checkContext(AOp, Args, NId, OpN, Geo, L),
   HwReqs =< HwCaps,
   placeFunctions(AOp, (SId, Node), ServicePlacement, P2, Placement, NewPlacement, Caps, NewCaps).

placeFunctions(AOp, (SId, Node), ServicePlacement, whl(FId, P), Placement, [(SId, FId, NId)|NewPlacement], Caps, NewCaps) :-
    func(FId, Args, HwReqs, PReqs, TUnits),
    node(NId, OpN, HwCaps, SPlats, FPlats, CostPU, Geo),
    trusts2(AOp, OpN),
    checkPlatforms(PReqs, FPlats),
    checkContext(AOp, Args, NId, OpN, Geo, L),
    HwReqs =< HwCaps,
    placeFunctions(AOp, (SId, Node), ServicePlacement, P, Placement, NewPlacement, Caps, NewCaps).

placeFunctions(AOp, (SId, Node), ServicePlacement, trc(P1, P2), Placement, NewPlacement, Caps, NewCaps) :-
    placeFunctions(AOp, (SId, Node), ServicePlacement, P1, Placement, PlacementTmp1, Caps, NewCaps1),
    placeFunctions(AOp, (SId, Node), ServicePlacement, P2, Placement, PlacementTmp2, Caps, NewCaps2),
    append(PlacementTmp1, PlacementTmp2, NewPlacement).

placeFunctions(AOp, (SId, Node), ServicePlacement, send(Args, Service, _), Placement, Placement, Caps, Caps) :-
    findNode(Service, TargetNode, ServicePlacement),
    labelF(AOp, Args, ReqSecurity),
    findRoute(AOp, 0, Latency, Node, Node, TargetNode, ReqSecurity, [Source | Route]).
    
placeFunctions(AOp, (SId, Node), ServicePlacement, read(Res, CD, COI, Var), Placement, Placement, Caps, Caps).
placeFunctions(AOp, (SId, Node), ServicePlacement, write(Var, Res, CD, COI), Placement, Placement, Caps, Caps).
placeFunctions(AOp, (SId, Node), ServicePlacement, new(Res, CD, COI, LRes), Placement, Placement, Caps, Caps).

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

findRoute(AOp, OldLatency, OldLatency, Old, Source, Source, _, []).
findRoute(AOp, OldLatency, Latency, Old, Source, Dest, ReqSecurity, [Source | Route]) :- 
    Source \== Dest,
    isConnected(Source, Step, L, LinkLatency),
    Old \== Step,
    OldLatency =< LinkLatency,
    labelL(AOp, L, ReqSecurity),
    findRoute(AOp, LinkLatency, Latency, Source, Step, Dest, ReqSecurity, Route).

findRoute(AOp, OldLatency, Latency, Old, Source, Dest, ReqSecurity, [Source | Route]) :- 
    Source \== Dest,
    isConnected(Source, Step, L, LinkLatency),
    Old \== Step,
    LinkLatency =< OldLatency,
    labelL(AOp, L, ReqSecurity),
    findRoute(AOp, OldLatency, Latency, Source, Step, Dest, ReqSecurity, Route).

isConnected(Source, Step, L, LinkLatency) :-
    link(L, LinkLatency, [Source, Step]); link(L, LinkLatency, [Step, Source]). 

computeCost(Placement, Cost) :- computeCost(Placement, 0, Cost).
computeCost([],Cost,Cost).
computeCost([(FId, NId)|Placement], Cost, NewCost) :-
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

% default lattice l <= s <= ts
leq(default, l, s).
leq(default, s, ts).

labelF(default, Args, ts).
labelF(default, rgs, s) :- findall(X, ts(X, Args), []).
labelF(default, Args, l) :- findall(X, notPublic(X, Args), []).
notPublic(X, Args) :-  member(X, Args),(ts(X);s(X)). 
ts(X, Args) :- member(X, Args), ts(X).

% labels a service composing multiple functions
% NOT NEEDED
%labelS(AOp, SId, L) :- service(SId, _, P, _, _, _), ctx(AOp, P, L, []).

labelL(AOp, L, Lbl) :- 
                    link(L, _, [N1, N2]),
                    node(N1, OpN, _, _, _, _, Geo),
                    node(N2, OpN, _, _, _, _, Geo),
                    labelN(AOp, N1, OpN, Geo, Lbl),
                    labelN(AOp, N2, OpN, Geo, Lbl).

% checks if node label supports function label
supports(AOp,NId,l) :- labelN(AOp,NId,_).
supports(AOp,NId,s) :- labelN(AOp,NId,L), L \== l.
supports(AOp,NId,ts) :- labelN(AOp,NId,ts). 

% default trust model à la SecFog
trusts(X,X).               
trusts2(A,B) :- trusts(A,B).   
trusts2(A,B) :- trusts(A,C),trusts2(C,B), A \== B.

%security context
ctx(_, tau, _ , _, _, _, _).
ctx(AOp, seq(P1, P2), L, Env, NewEnv, History, NewHistory) :- 
    ctx(AOp, P1, L, Env, TmpEnv, History, TmpHistory), 
    ctx(AOp, P2, L, TmpEnv, NewEnv, TmpHistory, NewHistory).
ctx(AOp, ife(FId, P1, _), L, Env, NewEnv, History, NewHistory) :-
   func(FId, Args, _, _, _),
   labelF(AOp, Args, L),
   union(Env, Args, TmpEnv),
   ctx(AOp, P1, L, TmpEnv, NewEnv, History, NewHistory).
ctx(AOp, ife(FId, _, P2), L, Env, NewEnv, History, NewHistory) :-
   func(FId, Args, _, _, _),
   labelF(AOp, Args, L),
   union(Env, Args, TmpEnv),
   ctx(AOp, P2, L, TmpEnv, NewEnv, History, NewHistory).
ctx(AOp, whl(FId, P), L, Env, NewEnv, History, NewHistory) :-
   func(FId, Args, _, _, _),
   labelF(AOp, Args, L),
   union(Env, Args, TmpEnv),
   ctx(AOp, P, L, TmpEnv, NewEnv, History, NewHistory).
ctx(AOp, trc(P1, P2), L, Env, NewEnv, History, NewHistory) :- 
    ctx(AOp, P1, L, Env, TmpEnv, History, TmpHistory), 
    ctx(AOp, P2, L, TmpEnv, NewEnv, TmpHistory, NewHistory).
ctx(AOp, FId, L, Env, NewEnv, History, History) :- 
    func(FId, Args, _, _, _),
    labelF(AOp, Args, L),
    union(Env, Args, NewEnv).
ctx(AOp, send(Args, Service, Timeout), L, Env, Env, History, History) :-
    responseTime(Service, Time),
    Time =< Timeout,
    subset(Args, Env),
    labelF(AOp, Args, L),
    service(Service, _, Prog, _, _, _),
    ctx(AOp, Prog, L2, Args, ServEnv, [], ServHistory),
    leq2(AOp, L, L2).
ctx(AOp, send(Args, Service, Timeout), L, Env, Env, History, History) :-
    responseTime(Service, Time),
    Timeout =< Time,
    subset(Args, Env),
    labelF(AOp, Args, L).
ctx(AOp, read(Res, CD, COI, Var), LVar, Env, Env, History, [(Res, CD, COI)|History]) :-
    labelResource(Res, CD, COI, LRes),
    canRead(Res, CD, COI, History),
    leq2(AOp, LRes, LVar),
    call(LVar, Var),
    assertz(labelResource(Res, CD, COI, LVar)).
% LVar <= LRes
ctx(AOp, write(Var, Res, CD, COI), LRes, Env, Env, History, History) :-
    labelResource(Res, CD, COI, LRes),
    canWrite(Res, CD, COI, History),
    leq2(AOp, LVar, LRes),
    call(LVar, Var),
    NewSecLevel =.. [LRes, Var],
    assertz(NewSecLevel).
ctx(AOp, new(Res, CD, COI, LRes), LRes, Env, Env, History, History) :-
    assertz(labelResource(Res, CD, COI, LRes)).
