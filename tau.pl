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

placeFunctions(AOp, SId, read(Res, Var), Placement, Placement, Caps, Caps).
placeFunctions(AOp, SId, write(Var, Res), Placement, Placement, Caps, Caps).
placeFunctions(AOp, SId, new(Res, LRes, CD, COI), Placement, Placement, Caps, Caps).


% ACCESS CONTROL
% canRead(appOp, file).
% canWrite(appOp, file).
% canRead(ann, file1).
% canWrite(ann, file1).
% canRead(ann, file2).

canRead(Res, CD, COI, History) :- 
    forall(member((O, OCD, OCOI), History), readCondition(O, OCD, OCOI, Res, CD, COI)).

readCondition(O, OCD, OCOI, Res, CD, COI) :- 
    O \== Res, (OCOI \== COI; OCD == CD).


canWrite(Res, CD, COI, History) :- 
    canRead(Res, CD, COI, History), 
    forall(member((O, OCD, OCOI), History), writeCondition(O, OCD, Res, CD)).

writeCondition(O, OCD, Res, CD) :- 
    O \== Res, OCD == CD.



% INFORMATION FLOW
% labelResource(appOp, res, cd, coi label).
labelResource(ann, log_file, cd1, coi1, ts).
labelResource(ann, temperature_sensor, cd2, coi1, l).


% better labelling needed
leq(AOp, X, X).               
leq2(AOp, A, B) :- leq(AOp, A, B).   
leq2(AOp, A, B) :- leq(AOp, A, C), leq2(AOp, C, B), A \== B.

% default lattice l <= s <= ts
leq(ann, l, s).
leq(ann, s, ts).

% LRes <= LVar
ctx(AOp, read(Res, Var), LVar, History, [(Res, CD, COI)|History]) :-
    labelResource(AOp, Res, CD, COI, LRes),
    canRead(Res, CD, COI, History),
    leq2(AOp, LRes, LVar),
    call(LVar, Var),
    assertz(labelResource(AOp, Res, CD, COI, LVar)).


% LVar <= LRes
ctx(AOp, write(Var, Res), LRes, History, History) :-
    labelResource(AOp, Res, CD, COI, LRes),
    canWrite(Res, CD, COI, History),
    leq2(AOp, LVar, LRes),
    call(LVar, Var),
    NewSecLevel =.. [LRes, Var],
    assertz(NewSecLevel).

ctx(AOp, new(Res, LRes, CD, COI), LRes, History, History) :-
    assertz(labelResource(AOp, Res, CD, COI, LRes)).

