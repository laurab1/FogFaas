:- use_module(library(lists)).

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

func(sum, [x,y], 1, rust, 10).
func(mult,[y,t], 1, java, 10).
func(div, [z,z], 2, python, 20).

%service(SId, Trigger, Program, HWReqs, PReqs, GeoReqList, TimeUnits).
service(service1, triggerX, seq(sum, mult), 1, python, [us]).
service(service2, triggerY, ife(sum, mult,div), 1, java, [fr]).

%app(OpA, AId, [SIds]).
app(bob, app1, [service1, service2]).

node(n1, amazon, 2, [python, rust, java, javascript], 0.001, eu).
encrypted_storage(n1).
firewall(n1).

node(n2, amazon, 2, [python, rust, java, javascript], 0.001, eu).
encrypted_storage(n2).
firewall(n2).

trusts(bob, amazon).

%app(OpA, AId, [SIds]).
app(bob, app1, [service1, service2]).

query(labelF(bob, [x, y], L)).
%query(placeFunctions(bob, ife(mult, sum, par(div, mult)), R, C)).