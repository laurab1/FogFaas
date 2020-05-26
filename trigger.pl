
ts(z).
s(x).
l(y).
l(t).



%trigger(TId, Prog, Rule).
trigger(triggerX, sum, rule1).
trigger(triggerY, div, rule2).
trigger(triggerZ, sum, rule3).



placeTriggers(AOp, [], T, T).
placeTriggers(AOp, [TId|Rest], Placement, [(TId, NId)|NewPlacement], Caps, NewCaps) :-
    trigger(TId, Prog, Rule),
    node(NId, OpN, HwCaps, SPlats, _, CostPU, NodeLoc),
    member(NodeLoc, Geo),
    subset(PReqs, SPlats),
    trusts2(AOp, OpN),
    checkHw(HwCaps, HwReqs, NId, Caps, TmpCaps),
    placeTriggers(AOp, Rest, Placement, NewPlacement, TmpCaps, NewCaps).


% INFORMATION FLOW
labelTrigger(AOp, TId, L) :- 
    trigger(TId, P, _), 
    ctx(AOp, P, L).


placeFunctions(AOp, SId, fireTrigger(TId), Placement, [(SId, FId, NId)|Placement], Caps, NewCaps) :-
    trigger(TId, FId, Rule),
    func(FId, Args, HwReqs, PReqs, TUnits),
    node(NId, OpN, HwCaps, SPlats, FPlats, CostPU, Geo),
    trusts2(AOp, OpN),
    checkPlatforms(PReqs, FPlats),
    checkContext(AOp, Args, NId, OpN, Geo, L),
    HwReqs =< HwCaps, checkHw(HwCaps, HwReqs, NId, Caps, NewCaps).  



ctxFire(AOp, fireTrigger(TId), L) :-
    trigger(TId, FId, _),
    func(FId, Args, _, _, _),
    labelF(AOp, Args, L). 