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

%%%%%%%%%%%%%%%% App %%%%%%%%%%%%%%%%

ts(z).
s(x).
l(y).
l(t).

% functions

func(sum, [x,y], 1, rust, 10).
func(mult,[y,t], 1, java, 10).
func(div, [z,z], 2, python, 20).

%service(SId, Trigger, Program, HWReqs, PReqs, GeoReqList, TimeUnits).
service(service1, triggerX, sum, 1, [ubuntu], [eu]).
service(service2, triggerY, div, 1, [sql], [eu]).

node(n1, amazon, 2, [ubuntu, sql], [python, rust, java, javascript], 0.001, eu).
encrypted_storage(n1).
firewall(n1).

node(n2, amazon, 1, [ubuntu, sql], [python, rust, java, javascript], 0.001, eu).
encrypted_storage(n2).
firewall(n2).

trusts(ann, amazon).

%app(OpA, AId, [SIds]).
app(app1, [service1, service2]).
