:- consult('app.pl').
:- consult('infra.pl').
:- consult('fogfaas.pl').

query(placeFunctions(default, (service1, n1), [(service1, n1),(service2,n2)], seq(mult,sum), [], R, [], C)).

query(placeServices(default, [service1, service2], [], P, [], C)).

query(placeApp(default, app1, SP, FP)).

%query(findNode(service2, N, [(service2, n2)])).

%query(findRoute(ann, 0, L, n1, n1, n3, s, R)).

%query(isConnected(N, n1, L, LinkLatency)).

%query(ctx(ann, seq(sum, send([x], service1, 1)), L, [])).

%query(ctx(default, ife(sum, sum, send([x], service1, 1)), L, [x], Env)).