% :- use_module(library(forall)).

% FILE OPERATIONS PLACEMENT
% placeFunctions(AOp, SId, read(Res, Var), Placement, [(SId, read(Res, Var), NId)|Placement], Caps, Caps) :-
%     node(NId, OpN, HwCaps, SPlats, FPlats, CostPU, Geo),
%     trusts2(AOp, OpN),
%     labelN(AOp, NId, OpN, Geo, L),
%     labelResource(AOp, Res, L).


% placeFunctions(AOp, SId, write(Var, File), Placement, [(SId, write(Var, File), NId)|Placement], Caps, Caps) :-
%     node(NId, OpN, HwCaps, SPlats, FPlats, CostPU, Geo),
%     trusts2(AOp, OpN),
%     labelN(AOp, NId, OpN, Geo, L),
%     labelFile(AOp, File, L).

placeFunctions(AOp, SId, read(Res, CD, COI, Var), Placement, Placement, Caps, Caps).
placeFunctions(AOp, SId, write(Var, Res, CD, COI), Placement, Placement, Caps, Caps).
placeFunctions(AOp, SId, new(Res, CD, COI, LRes), Placement, Placement, Caps, Caps).


% ACCESS CONTROL

canRead(Res, CD, COI, []).
canRead(Res, CD, COI, [(O, OCD, OCOI)|Rest]) :- 
    readCondition(O, OCD, OCOI, Res, CD, COI), 
    canRead(Res, CD, COI, Rest).

% canRead(Res, CD, COI, History) :- 
%     forall(member((O, OCD, OCOI), History), readCondition(O, OCD, OCOI, Res, CD, COI)).


readCondition(O, OCD, OCOI, Res, CD, COI) :- 
    O \== Res, (OCOI \== COI; OCD == CD).


canWrite(Res, CD, COI, []).
canWrite(Res, CD, COI, [(O, OCD, OCOI)|Rest]) :- 
    writeCondition(O, OCD, OCOI, Res, CD, COI), 
    canWrite(Res, CD, COI, Rest).

% canWrite(Res, CD, COI, History) :- 
%     forall(member((O, OCD, OCOI), History), writeCondition(O, OCD, OCOI, Res, CD, COI)).

writeCondition(O, OCD, OCOI, Res, CD, COI) :-
    readCondition(O, OCD, OCOI, Res, CD, COI), OCD == CD.



% INFORMATION FLOW
% labelResource(res, cd, coi label).
% labelResource(log_file, cd1, coi1, ts).
% labelResource(temperature_sensor, cd2, coi1, l).


% LRes <= LVar
ctx(AOp, read(Res, CD, COI, Var), LVar, History, [(Res, CD, COI)|History]) :-
    labelResource(Res, CD, COI, LRes),
    canRead(Res, CD, COI, History),
    leq2(AOp, LRes, LVar),
    call(LVar, Var),
    assertz(labelResource(Res, CD, COI, LVar)).


% LVar <= LRes
ctx(AOp, write(Var, Res, CD, COI), LRes, History, History) :-
    labelResource(Res, CD, COI, LRes),
    canWrite(Res, CD, COI, History),
    leq2(AOp, LVar, LRes),
    call(LVar, Var),
    NewSecLevel =.. [LRes, Var],
    assertz(NewSecLevel).

ctx(AOp, new(Res, CD, COI, LRes), LRes, History, History) :-
    assertz(labelResource(Res, CD, COI, LRes)).

