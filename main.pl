:- consult('app.pl').
:- consult('infra.pl').
:- consult('fogfaas.pl').

%query(placeFunctions(default, (service1, n1), [(service1, n1),(service2,n2), (service3, n3)], send([x], service3, 1), [], R, [], C)).

%query(placeServices(default, [webserver, placesService], [], P, [], C)).

query(placeApp(default, app1, SP, FP)).

%query(computeCost(FP, Cost)).

%query(placeAllFunctions(default, [(webserver, n1), (placesService, n5)], [(webserver, n1), (placesService, n5)], [], NewFPlacement, NewCaps)).

%query(findRoute(default, 0, L, n1, n1, n3, s, R)).

%query(isConnected(N, n1, L, LinkLatency)).

%query(ctx(default, seq(write(place, places, asl, files), fireTrigger(triggerZ)), L, [], Env, [], Hist)).

%query(ctx(default, ife(sum, sum, send([x], service1, 1)), L, [x], Env)).