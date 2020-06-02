func(sum, [x,z], 1, rust, 10).
func(mult,[y,vz], 1, java, 10).
func(div, [z,z], 2, python, 20).
func(true, [], 1, python, 5).

0.6::trigger(service3, triggerX, sum).
0.4::trigger(service2, triggerX, sum).
trigger(service2, triggerY, div).
0.2::trigger(service3, triggerZ, div).
0.8::trigger(service2, triggerZ, div).

rule(triggerX, [eu]).
rule(triggerY, [us]).
rule(triggerZ, [eu, us]).

%service(SId, Trigger, Program, HWReqs, PReqs, GeoReqList, TimeUnits).
service(service1, triggerX, div, 1, [ubuntu], [us]).
service(service2, triggerY, div, 1, [sql], [us]).
service(service3, triggerZ, seq(sum, send([x], service1, 1)), 1, [sql], [us]).

0.7::responseTime(service1, 0.5).
0.3::responseTime(service1, 2).

0.7::responseTime(service3, 0.5).
0.3::responseTime(service3, 2).

%app(AId, [SIds]).
app(app1, [service1, service2, service3]).