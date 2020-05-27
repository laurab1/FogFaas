node(n1, amazon, 5, [ubuntu], [python, rust], 0.003, eu).
encrypted_storage(n1).
firewall(n1).

node(n2, amazon, 6, [ubuntu, sql], [python, rust, kotlin], 0.001, us).
encrypted_storage(n2).
firewall(n2).

node(n3, ibm, 3, [sql], [kotlin, java, javascript], 0.003, eu).
firewall(n3).

node(n4, amazon, 4, [ubuntu, sql], [python, kotlin], 0.001, us).
encrypted_storage(n4).
firewall(n4).

node(n5, amazon, 7, [ubuntu, sql], [python, rust, java, javascript], 0.001, eu).
encrypted_storage(n5).
firewall(n5).

node(n6, azure, 4, [ubuntu, sql], [python, kotlin], 0.004, eu).
encrypted_storage(n6).
firewall(n6).

node(n7, azure, 6, [ubuntu, sql], [python, kotlin], 0.004, eu).
encrypted_storage(n7).
firewall(n7).

%link(l1, 1, [n1, n2]).
0.7::link(l1, 1, [n1, n2]).
0.3::link(l1, 3, [n1, n2]).
link(l2, 1, [n2, n3]).
link(l3, 1, [n1, n4]).
0.3::link(l4, 1, [n3, n4]).
0.7::link(l4, 3, [n3, n4]).
0.5::link(l5, 2, [n4, n5]).
0.5::link(l5, 3, [n4, n5]).
link(l6, 1, [n5, n6]).
%link(l4, 1, [n3, n4]).