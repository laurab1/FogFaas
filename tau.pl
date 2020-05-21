% canRead(appOp, file).
% canWrite(appOp, file).
canRead(ann, file1).
canWrite(ann, file1).
canRead(ann, file2).

% labelFile(appOp, file, label).
labelFile(ann, file1, ts).
labelFile(ann, file2, l).

placeFunctions(AOp, SId, read(File, Var), Placement, [(SId, read(File, Var), NId)|Placement], Caps, NewCaps) :-
    node(NId, OpN, HwCaps, SPlats, FPlats, CostPU, Geo),
    trusts2(AOp, OpN),
    labelN(AOp, NId, OpN, Geo, L),
    labelFile(AOp, File, L),
    1 =< HwCaps, checkHw(HwCaps, 1, NId, Caps, NewCaps). 


placeFunctions(AOp, SId, write(Var, File), Placement, [(SId, read(File, Var), NId)|Placement], Caps, NewCaps) :-
    node(NId, OpN, HwCaps, SPlats, FPlats, CostPU, Geo),
    trusts2(AOp, OpN),
    labelN(AOp, NId, OpN, Geo, L),
    labelFile(AOp, File, L),
    1 =< HwCaps, checkHw(HwCaps, 1, NId, Caps, NewCaps).


% better labelling needed

ctx(AOp, read(File, Var), L) :-
    canRead(AOp, File), % AC
    labelFile(AOp, File, L),
    call(L, Var). % ???

ctx(AOp, write(Var, File), L) :-
    canWrite(appOp, File), % AC
    labelFile(AOp, File, L),
    call(L, Var). % ???

