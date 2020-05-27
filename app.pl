%%%%%%%%%%%%%%%% User defined predicates %%%%%%%%%%%%%%%%
% labels a node with its security context
labelN(default, N, OpN, Geo, ts) :- 
                            member(Geo, [eu,ch]), 
                            firewall(N), 
                            member(OpN, [amazon, azure]).
labelN(default, N, OpN, Geo, s) :- 
                            member(Geo, [eu,ch,us]).
labelN(default, N, OpN, Geo, l) :- 
                            member(Geo, [eu,ch,us,vat]).


%%%%%%%%%%%%%%%% App %%%%%%%%%%%%%%%%

ts(pos1).
ts(pos2).
l(place).
s(x).
ts(z).

0.6::trusts(amazon, azure).
0.2::trusts(amazon, ibm).

% functions

func(notification, [], 3, kotlin, 10).
func(distance, [pos1, pos2], 4, python, 20).
func(true, [], 1, rust, 5).
func(computePlace, [place], 2, python, 10).
func(formatData, [pos1], 4, java, 10).

0.5::trigger(contactsService, triggerZ, notification).

rule(triggerZ, [eu, us]).

labelResource(position, android, sensors, s).
labelResource(contacts_log, asl, files, ts).
labelResource(places, asl, files, ts).

%service(SId, Trigger, Program, HWReqs, PReqs, GeoReqList, TimeUnits).
service(webserver, triggerX,  whl(true, seq(read(position, android, sensors, pos1), seq(formatData, send([pos1], contactsService, 1)))), 3, [ubuntu], [eu]).
service(contactsService, triggerY, whl(true, seq(read(contacts_log, asl, files, pos2), ife(distance, seq(computePlace, send([place], placesService, 0.8)), computePlace))), 2, [ubuntu, sql], [eu]).
service(placesService, triggerZ, seq(write(place, places, asl, files), fireTrigger(triggerZ)), 2, [sql], [eu, us]).

0.7::responseTime(contactsService, 0.5).
0.3::responseTime(contactsService, 2).
0.4::responseTime(placesService, 0.3).
0.4::responseTime(placesService, 0.7).
0.2::responseTime(placesService, 1).

%app(OpA, AId, [SIds]).
app(app1, [webserver, placesService, contactsService]).
