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

%%%%%%%%% Working (problog) code %%%%%%%%%

:- consult('utils.pl').

placeServices(AOp, [], P, P, C, C).
placeServices(AOp, [SId|Rest], Placement, [(SId, NId)|NewPlacement], Caps, NewCaps) :-
    service(SId, Trig, Prog, HwReqs, PReqs, Geo),
    ctx(AOp, Prog, L, [], Env, [], History),
    node(NId, OpN, HwCaps, SPlats, _, CostPU, NodeLoc),
    member(NodeLoc, Geo),
    subset(PReqs, SPlats),
    trusts2(AOp, OpN),
    checkHw(HwCaps, HwReqs, NId, Caps, TmpCaps),
    placeServices(AOp, Rest, Placement, NewPlacement, TmpCaps, NewCaps).

placeApp(AOp, AId, ServicePlacement, FunctionPlacement, Cost):-
    app(AId, Services),
    placeServices(AOp, Services, [], ServicePlacement, [], Caps),
    placeAllFunctions(AOp, ServicePlacement, ServicePlacement, [], FunctionPlacement, Caps),
    computeCost(FunctionPlacement, Cost).

placeAllFunctions(_, [], _, FP, FP, _).
placeAllFunctions(AOp, [(SId, Node)|Placement], GlobPlacement, FPlacement, NewFPlacement, Caps) :-
    service(SId, TId, Prog, _, _, _),
    placeFunctions(AOp, (SId, Node), GlobPlacement, Prog, FPlacement, TmpFPlacement, Caps, NewCaps),
    placeAllFunctions(AOp, Placement, GlobPlacement, TmpFPlacement, NewFPlacement, NewCaps).

placeFunctions(_, _, _, tau, _, [], _, _).

placeFunctions(AOp, (SId, Node), ServicePlacement, par(F1, F2), Placement, NewPlacement, Caps, NewCaps) :-
      placeParFunctions(AOp, (SId, Node), ServicePlacement, F1, Placement, PlacementTmp, Caps, CapsTmp),
      placeParFunctions(AOp, (SId, Node), ServicePlacement, F2, PlacementTmp, NewPlacement, CapsTmp, NewCaps).

placeParFunctions(AOp, (SId, Node), ServicePlacement, FId, Placement, [(SId, FId, NId)|Placement], Caps, NewCaps) :-
      func(FId, Args, HwReqs, PReqs, TUnits),
      node(NId, OpN, HwCaps, SPlats, FPlats, CostPU, Geo),
      trusts2(AOp, OpN),
      checkPlatforms(PReqs, FPlats),
      checkContext(AOp, Args, NId, OpN, Geo, L),
      HwReqs =< HwCaps, checkHw(HwCaps, HwReqs, NId, Caps, NewCaps).   

placeFunctions(AOp, (SId, Node), ServicePlacement, seq(P1, P2), Placement, NewPlacement, Caps, NewCaps) :-
      placeFunctions(AOp, (SId, Node), ServicePlacement, P1, Placement, PlacementTmp1, Caps, NewCaps),
      placeFunctions(AOp, (SId, Node), ServicePlacement, P2, Placement, PlacementTmp2, Caps, NewCaps),
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

placeFunctions(AOp, (SId, Node), ServicePlacement, fireTrigger(TId), Placement, [(SId, FId, NId)|Placement], Caps, Caps) :-
    trigger(Service, TId, FId),
    func(FId, Args, HwReqs, PReqs, TUnits),
    node(NId, OpN, HwCaps, SPlats, FPlats, CostPU, Geo),
    trusts2(AOp, OpN),
    checkPlatforms(PReqs, FPlats),
    checkContext(AOp, Args, NId, OpN, Geo, L),
    HwReqs =< HwCaps, checkHw(HwCaps, HwReqs, NId, Caps, NewCaps).  

%%%%%%%%%%% DEFAULT OPTIONS %%%%%%%%%%%

leq(default, l, s).
leq(default, s, ts).

labelF(default, Args, ts).
labelF(default, Args, s) :- findall(X, ts(X, Args), []).
labelF(default, Args, l) :- findall(X, notPublic(X, Args), []).
notPublic(X, Args) :-  member(X, Args),(ts(X);s(X)). 
ts(X, Args) :- member(X, Args), ts(X).

guardCheck(default, l).

trusts(default, amazon).

%security context
ctx(_, tau, _ , _, _, _, _).
ctx(AOp, par(F1, F2), L, Env, NewEnv, History, History) :- 
    func(F1, Args1, _, _, _),
    func(F2, Args2, _, _, _),
    labelF(AOp, Args1, L),
    labelF(AOp, Args2, L),
    union(Env, Args1, TmpEnv),
    union(TmpEnv, Args2, NewEnv).
ctx(AOp, seq(P1, P2), L, Env, NewEnv, History, NewHistory) :- 
    ctx(AOp, P1, L, Env, TmpEnv, History, TmpHistory), 
    ctx(AOp, P2, L, TmpEnv, NewEnv, TmpHistory, NewHistory).
ctx(AOp, ife(FId, P1, P2), L, Env, NewEnv, History, NewHistory) :-
   func(FId, Args, _, _, _),
   checkTime(ife(FId, P1, P2), _),
   labelF(AOp, Args, L),
   union(Env, Args, TmpEnv),
   ctx(AOp, P1, L, TmpEnv, NewEnv, History, NewHistory).
ctx(AOp, ife(FId, P1, P2), L, Env, NewEnv, History, NewHistory) :-
   func(FId, Args, _, _, _),
   checkTime(ife(FId, P1, P2), _),
   labelF(AOp, Args, L),
   union(Env, Args, TmpEnv),
   ctx(AOp, P2, L, TmpEnv, NewEnv, History, NewHistory).
ctx(AOp, whl(FId, P), L, Env, NewEnv, History, NewHistory) :-
   func(FId, Args, _, _, _),
   labelF(AOp, Args, L1),
   guardCheck(AOp, L1),
   leq2(AOp, L1, L),
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
ctx(AOp, fireTrigger(TId), L, Env, Env, History, History) :-
    trigger(SId, TId, FId),
    func(FId, Args, _, _, _),
    labelF(AOp, Args, L),
    service(SId, _, Prog, _, _, Geo),
    rule(TId, Geos),
    subset(Geo, Geos),
    ctx(AOp, Prog, L2, [], ServEnv, [], ServHistory),
    leq2(AOp, L2, L). 
