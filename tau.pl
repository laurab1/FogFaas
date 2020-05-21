% FILE OPERATIONS PLACEMENT
placeFunctions(AOp, SId, read(File, Var), Placement, [(SId, read(File, Var), NId)|Placement], Caps, NewCaps) :-
    node(NId, OpN, HwCaps, SPlats, FPlats, CostPU, Geo),
    trusts2(AOp, OpN),
    labelN(AOp, NId, OpN, Geo, L),
    labelFile(AOp, File, L),
    1 =< HwCaps, checkHw(HwCaps, 1, NId, Caps, NewCaps). 


placeFunctions(AOp, SId, write(Var, File), Placement, [(SId, write(Var, File), NId)|Placement], Caps, NewCaps) :-
    node(NId, OpN, HwCaps, SPlats, FPlats, CostPU, Geo),
    trusts2(AOp, OpN),
    labelN(AOp, NId, OpN, Geo, L),
    labelFile(AOp, File, L),
    1 =< HwCaps, checkHw(HwCaps, 1, NId, Caps, NewCaps).

% ACCESS CONTROL
% canRead(appOp, file).
% canWrite(appOp, file).
canRead(ann, file1).
canWrite(ann, file1).
canRead(ann, file2).

% INFORMATION FLOW
% labelFile(appOp, file, label).
labelFile(ann, file1, ts).
labelFile(ann, file2, l).


% better labelling needed
leq(X,X).               
leq2(A,B) :- leq(A,B).   
leq2(A,B) :- leq(A,C), leq2(C,B), A \== B.

% default lattice l <= s <= ts
leq(l, s).
leq(s, ts).

% LFile <= LVar
ctx(AOp, read(File, Var), LVar) :-
    canRead(AOp, File), % AC
    labelFile(AOp, File, LFile),
    leq(LFile, LVar),
    call(LVar, Var).

% LVar <= LFile
ctx(AOp, write(Var, File), LFile) :-
    canWrite(AOp, File), % AC
    labelFile(AOp, File, LFile),
    leq(LVar, LFile),
    call(LVar, Var).

