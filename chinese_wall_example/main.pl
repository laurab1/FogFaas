:- consult('app.pl').
:- consult('infra.pl').
:- consult('../fogfaas.pl').

query(placeApp(default, my_app, SP, FP)).
% query(ctx(default, format_data, L, [], Env, [], History)).