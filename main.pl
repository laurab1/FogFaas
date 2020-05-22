:- consult('app.pl').
:- consult('infra.pl').
:- consult('fogfaas.pl').

%query(placeFunctions(ann, (service1, n1), [(service1, n1),(service2,n4)], send([x], service2), [], R, [], C)).

query(placeApp(ann, app1, SP, FP)).

%query(findNode(service2, N, [(service2, n2)])).

%query(labelC(ann, c1, s)).

query(findRoute(ann, 0, L, n1, n1, n3, s, R)).

%query(isConnected(N, n1, L, LinkLatency)).