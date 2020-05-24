%%%%%%%%%%%%%%%% User defined predicates %%%%%%%%%%%%%%%%
% labels a node with its security context
labelN(ann, N, OpN, Geo, ts) :- 
                            member(Geo, [eu,ch]), 
                            firewall(N), 
                            member(OpN, [amazon, azure]).
labelN(ann, N, OpN, Geo, s) :- 
                            member(Geo, [eu,ch,us]).
labelN(ann, N, OpN, Geo, l) :- 
                            member(Geo, [eu,ch,us,vat]).

labelF(ann, Args, ts).
labelF(ann, Args, s) :- 
                    findall(X, ts(X, Args), []).
labelF(ann, Args, l) :- 
                    findall(X, notPublic(X, Args), []).
notPublic(X, Args) :-  
                    member(X, Args),(ts(X);s(X)). 
ts(X, Args) :- 
                    member(X, Args), ts(X).

labelL(ann, L, Lbl) :- 
                    link(L, _, [N1, N2]),
                    node(N1, OpN, _, _, _, _, Geo),
                    node(N2, OpN, _, _, _, _, Geo),
                    labelN(ann, N1, OpN, Geo, Lbl),
                    labelN(ann, N2, OpN, Geo, Lbl).

%%%%%%%%%%%%%%%% App %%%%%%%%%%%%%%%%

ts(z).
s(x).
l(y).
l(t).

trusts(ann, amazon).

% functions

func(sum, [x,y], 1, rust, 10).
func(mult,[y,t], 1, java, 10).
func(div, [z,z], 2, python, 20).
func(true, [], 1, python, 5).

%service(SId, Trigger, Program, HWReqs, PReqs, GeoReqList, TimeUnits).
service(service1, triggerX, seq(div,send([z], service3, 1)), 1, [ubuntu], [eu]).
service(service2, triggerY, div, 1, [sql], [eu]).
service(service3, triggerX, send([x], service1, 1), 1, [ubuntu], [eu]).

responseTime(service1, 0.5).
%0.3::responseTime(service1, 2).

0.7::responseTime(service3, 0.5).
0.3::responseTime(service3, 2).

%app(OpA, AId, [SIds]).
app(app1, [service1, service2, service3]).
