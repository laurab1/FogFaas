:- consult('app.pl').
:- consult('infra.pl').
:- consult('fogfaas.pl').

%query(placeFunctions(ann, service1, seq(mult, div), [], R, [], C)).

%query(placeApp(ann, app1, SP, FP)).

%query(labelC(ann, c1, s)).

query(findRoute(ann, 2, n1, n1, n3, s, R)).

%query(isConnected(N, n1, L, LinkLatency)).