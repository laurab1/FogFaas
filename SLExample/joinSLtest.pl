:- use_module(library(lists)).
:- consult('../fogfaas.pl').

%%%%%%%%%%%%%%%% User-defined semilattice %%%%%%%%%%%%%%%%

l(x).
s(y, eu).
s(z, us).
ts(v, eu).
ts(w, us).

guardCheck(bob, l).

labelF(bob, Args, ts_eu) :- findall(X, isUs(X, Args), []).
labelF(bob, Args, ts_us) :- findall(X, isEu(X, Args), []).
isUs(X, Args) :- member(X, Args),(s(X, us); ts(X, us)).
isEu(X, Args) :- member(X, Args),(s(X, eu); ts(X, eu)).

labelF(bob, Args, l) :- findall(X, isSec(X, Args), []).
isSec(X, Args) :- member(X, Args),(isUs(X, Args); isEu(X, Args)).

labelF(bob, Args, s_eu) :- findall(X, euOrTS(X, Args), []).
euOrTS(X, Args) :- member(X, Args),(isUs(X, Args); ts(X, eu)).

labelF(bob, Args, s_us) :- findall(X, usOrTS(X, Args), []).
usOrTS(X, Args) :- member(X, Args),(isEu(X, Args); ts(X, us)).

% default lattice l <= s <= ts
leq(bob, l, s_eu).
leq(bob, l, s_us).
leq(bob, s_eu, ts_eu).
leq(bob, s_us, ts_us).

labelN(bob, N, OpN, Geo, ts_eu) :- 
                            member(Geo, [eu]), 
                            firewall(N), 
                            member(OpN, [amazon, azure]).
labelN(bob, N, OpN, Geo, ts_us) :- 
                            member(Geo, [us]), 
                            firewall(N), 
                            member(OpN, [amazon, azure]).
labelN(bob, N, OpN, Geo, s_eu) :- 
                            member(Geo, [eu]).
labelN(bob, N, OpN, Geo, s_us) :- 
                            member(Geo, [us]).
labelN(bob, N, OpN, Geo, l) :- 
                            member(Geo, [eu,ch,us,vat]).

trusts(bob, amazon).