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

s(pos1).
s(pos2).
l(x).
ts(z).

0.6::trusts(amazon, azure).
0.2::trusts(amazon, ibm).

% functions

func(notification, [], 3, kotlin, 10).
func(distance, [pos1, pos2], 2, python, 20).
func(true, [x], 1, python, 5).

0.5::trigger(contactsService, triggerZ, notification).

rule(triggerZ, [eu, us]).

labelResource(position, android, sensors, s).
labelResource(contacts_log, asl, files, ts).
labelResource(places, asl, files, l).

%service(SId, Trigger, Program, HWReqs, PReqs, GeoReqList, TimeUnits).
service(webserver, triggerX, whl(true, seq(read(position, android, sensors, pos1), send(contactsService, [pos1], 1))), 3, [ubuntu], [eu]).
service(contactsService, triggerY, seq(read(contacts_log, asl, files, pos2), seq(ife(distance, write(pos1, contacts_log, asl), tau), send(placesService, [pos1], 0.8))), 4, [ubuntu, sql], [eu]).
service(placesService, triggerZ, seq(write(pos1, places, asl, files), fireTrigger(triggerZ)), 2, [sql], [eu, us]).

0.7::responseTime(contactsService, 0.5).
0.3::responseTime(contactsService, 2).
0.4::responseTime(placesService, 0.3).
0.4::responseTime(placesService, 0.7).
0.2::responseTime(placesService, 1).

%app(OpA, AId, [SIds]).
app(app1, [webserver, contactsService, placesService]).
