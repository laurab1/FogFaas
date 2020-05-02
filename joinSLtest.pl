:- use_module(library(lists)).

l(x).
l(t).
s_eu(y).
s_us(z).

labelF(bob, Args, l).

labelF(bob, Args, s_eu) :- findall(X,notEu(X, Args), []).
notEu(X, Args) :- member(X, Args),(s_us(X);l(X)).

labelF(bob, Args, s_us) :- findall(X,notUs(X, Args), []).
notUs(X, Args) :- member(X, Args),(s_eu(X);l(X)).

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

query(labelF(bob, [y], L)).
%query(placeFunctions(bob, ife(mult, sum, par(div, mult)), R, C)).