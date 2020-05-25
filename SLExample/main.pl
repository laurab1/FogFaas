:- consult('../fogfaas.pl').
:- consult('joinSLtest.pl').
:- consult('infra.pl').
:- consult('app.pl').

query(labelF(bob, [y], L)).

query(placeApp(bob, app1, SP, FP)).