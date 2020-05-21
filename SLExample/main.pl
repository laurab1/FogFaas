:- consult('../fogfaas.pl').
:- consult('joinSLtest.pl').
:- consult('infra.pl').
:- consult('app.pl').

query(labelF(bob, [y], L)).

query(placeApp(bob, app1, SP, FP)).
%query(placeFunctions(bob, ife(mult, sum, par(div, mult)), R, C)).