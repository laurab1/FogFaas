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
placeFunctions(AOp, SId, new(Res, LRes), Placement, Placement, Caps, Caps).


% ACCESS CONTROL
% canRead(appOp, file).
% canWrite(appOp, file).
canRead(ann, file1).
canWrite(ann, file1).
canRead(ann, file2).

% INFORMATION FLOW
% labelResource(appOp, res, label).
labelResource(ann, log_file, ts).
labelResource(ann, temperature_sensor, l).


% better labelling needed
leq(AOp, X, X).               
leq2(AOp, A, B) :- leq(AOp, A, B).   
leq2(AOp, A, B) :- leq(AOp, A, C), leq2(AOp, C, B), A \== B.

% default lattice l <= s <= ts
leq(ann, l, s).
leq(ann, s, ts).

% LRes <= LVar
ctx(AOp, read(Res, Var), LVar) :-
    % canRead(AOp, Res), % AC
    labelResource(AOp, Res, LRes),
    leq2(AOp, LRes, LVar),
    call(LVar, Var),
    assertz(labelResource(AOp, Res, LVar)).


% LVar <= LRes
ctx(AOp, write(Var, Res), LRes) :-
    % canWrite(AOp, Res), % AC
    labelResource(AOp, Res, LRes),
    leq2(AOp, LVar, LRes),
    call(LVar, Var),
    NewSecLevel =.. [LRes, Var],
    assertz(NewSecLevel).

ctx(AOp, new(Res, LRes), LRes) :-
    assertz(labelResource(AOp, Res, LRes)).

