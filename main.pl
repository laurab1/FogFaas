:- consult('app.pl').
:- consult('infra.pl').
:- consult('fogfaas.pl').

%query(placeFunctions(default, (service1, n1), [(service1, n1),(service2,n2), (service3, n3)], send([x], service3, 1), [], R, [], C)).

%query(placeServices(default, [webserver, placesService], [], P, [], C)).

query(placeApp(default, app1, SP, FP)).

%query(placeAllFunctions(default, [(webserver, n1), (placesService, n5)], [(webserver, n1), (placesService, n5)], [], NewFPlacement, NewCaps)).

%query(findRoute(default, 0, L, n1, n1, n3, s, R)).

%query(isConnected(N, n1, L, LinkLatency)).

query(ctx(default, whl(true, seq(read(position, android, sensors, pos1), seq(formatData, send([pos1], contactsService, 1)))), L, [], Env, [], Hist)).

%query(ctx(default, ife(sum, sum, send([x], service1, 1)), L, [x], Env)).