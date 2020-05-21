:- use_module(library(lists)).
:- consult('../fogfaas.pl').

%%%%%%%%%%%%%%%% User-defined semilattice %%%%%%%%%%%%%%%%

l(x).
s(y, eu).
s(z, us).
ts(v, eu).
ts(w, us).

labelF(bob, Args, ts_eu) :- findall(X, isUs(X, Args), []).
labelF(bob, Args, ts_us) :- findall(X, isEu(X, Args), []).
isUs(X, Args) :- member(X, Args),(s(X, us); ts(X, us)).
isEu(X, Args) :- member(X, Args),(s(X, eu); ts(X, eu)).

labelF(bob, Args, l) :- findall(X, notSec(X, Args), []).
notSec(X, Args) :- member(X, Args),(isUs(X, Args); isEu(X, Args)).

labelF(bob, Args, s_eu) :- findall(X, euNotTS(X, Args), []).
euNotTS(X, Args) :- member(X, Args),(isUs(X, Args); ts(X, eu)).

labelF(bob, Args, s_us) :- findall(X, usNotTS(X, Args), []).
usNotTS(X, Args) :- member(X, Args),(isEu(X, Args); ts(X, us)).

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