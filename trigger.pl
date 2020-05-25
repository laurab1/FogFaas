

trigger(triggerX, sum, rule1).
trigger(triggerY, div, rule2).
trigger(triggerY, sum, rule3).

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