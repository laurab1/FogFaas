%%% % TRIGGER PLACEMENT
%%% trigger(TId, FId, EventTime) :- 
%%%     checkCondition(EventTime, TUnits),
%%%     func(FId, Args, HwReqs, PReqs, TUnits).


%%% % CHECK CONDITION
%%% checkCondition(EventTime, TUnits) :- 
%%%     EventTime <= TUnits



trigger(triggerX, sum, 10).
trigger(triggerY, mult, 20).



placeTriggers(AOp, [], T, T).
placeTriggers(AOp, [TId|Rest], Placement, [(TId, NId)|NewPlacement]) :-
    trigger(TId, Prog, ETime),
    node(NId, OpN, HwCaps, SPlats, _, CostPU, NodeLoc),
    member(NodeLoc, Geo),
    subset(PReqs, SPlats),
    trusts2(AOp, OpN),
    checkHw(HwCaps, HwReqs, NId, Caps, TmpCaps),
    placeTriggers(AOp, Rest, Placement, NewPlacement).




% INFORMATION FLOW
labelTrigger(AOp, TId, L) :- 
    trigger(TId, P, _), 
    ctx(AOp, P, L).