:- consult('../fogfaas.pl').
:- consult('joinSLtest.pl').
:- consult('infra.pl').
:- consult('app.pl').

query(labelF(bob, [y], L)).

%query(placeFunctions(bob, (service1, n1), [(service1, n1),(service2,n2)], par(sum, mult), [], R, [], C)).

%query(placeServices(bob, [service1, service2], [], P, [], C)).

query(placeApp(bob, app1, SP, FP)).