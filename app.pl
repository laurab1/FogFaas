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

ts(z).
s(x).
l(y).
l(t).

% functions

func(sum, [x,y], 1, rust, 10).
func(mult,[y,t], 1, java, 10).
func(div, [z,z], 2, python, 20).
func(true, [], 1, python, 5).

trigger(service3, triggerX, sum).
trigger(service1, triggerY, div).
trigger(service2, triggerZ, sum).

%service(SId, Trigger, Program, HWReqs, PReqs, GeoReqList, TimeUnits).
service(service1, triggerX, seq(sum, fireTrigger(triggerX)), 1, [ubuntu], [eu]).
service(service2, triggerY, div, 1, [sql], [eu]).
service(service3, triggerX, seq(sum, send([x], service1, 1)), 1, [ubuntu], [eu]).

0.7::responseTime(service1, 0.5).
0.3::responseTime(service1, 2).

%0.7::responseTime(service3, 0.5).
%0.3::responseTime(service3, 2).

%app(OpA, AId, [SIds]).
app(app1, [service1, service2, service3]).
