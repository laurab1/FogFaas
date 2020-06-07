node(n1, amazon, 4, [ubuntu], [python, java, rust], 0.003, eu).
encrypted_storage(n1).
firewall(n1).

node(n2, amazon, 2, [ubuntu, sql], [python, rust, kotlin], 0.001, eu).
firewall(n2).

node(n3, azure, 3, [ubuntu, sql], [kotlin, java, javascript], 0.003, eu).
encrypted_storage(n3).
firewall(n3).

node(n4, amazon, 4, [ubuntu], [python, kotlin], 0.001, eu).
encrypted_storage(n4).
firewall(n4).

node(n5, azure, 3, [ubuntu, sql], [python, rust, java, javascript], 0.001, us).
firewall(n5).

node(n6, ibm, 2, [ubuntu, sql], [python, kotlin], 0.004, us).
firewall(n6).

0.7::link(l1, 1, [n1, n2]).
0.3::link(l1, 3, [n1, n2]).
link(l2, 1, [n2, n3]).
link(l3, 1, [n1, n4]).
0.3::link(l4, 1, [n3, n4]).
0.7::link(l4, 3, [n3, n4]).
0.5::link(l5, 2, [n4, n5]).
0.5::link(l5, 3, [n4, n5]).
link(l6, 1, [n3, n5]).
link(l7, 1, [n4, n6]).
link(l8, 1, [n3, n6]).
