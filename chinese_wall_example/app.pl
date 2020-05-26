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

labelL(default, L, s) :- 
    link(L, _, [N1, N2]),
    node(N1, OpN, _, _, _, _, Geo),
    node(N2, OpN, _, _, _, _, Geo),
    labelN(default, N1, OpN, Geo, s),
    labelN(default, N2, OpN, Geo, s).

%%%%%%%%%%%%%%%% App %%%%%%%%%%%%%%%%

% trust
trusts(default, amazon).

% variables
ts(x).

% resources
labelResource(new_car_design, ford, cars, ts).
labelResource(secret_car_project, toyota, cars, ts).

% functions
func(foo, [x, y], 1, java, 10).

%service(SId, Trigger, Program, HWReqs, PReqs, GeoReqList, TimeUnits).
service(my_service, my_trigger,
    seq(read(new_car_design, ford, cars, x), 
        write(x, secret_car_project, toyota, cars)), 1, [ubuntu], [eu]).


%app(OpA, AId, [SIds]).
app(my_app, [my_service]).
