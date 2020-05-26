:- consult('app.pl').
:- consult('infra.pl').
:- consult('../fogfaas.pl').

query(placeApp(default, my_app, SP, FP)).