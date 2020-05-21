func(sum, [x,z], 1, rust, 10).
func(mult,[y,v], 1, java, 10).
func(div, [z,z], 2, python, 20).

%service(SId, Trigger, Program, HWReqs, PReqs, GeoReqList, TimeUnits).
service(service1, triggerX, sum, 1, [ubuntu], [us]).
service(service2, triggerY, div, 1, [sql], [us]).

%app(AId, [SIds]).
app(app1, [service1, service2]).