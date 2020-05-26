node(n1, amazon, 2, [ubuntu, sql], [python, rust, java, javascript], 0.001, eu).
encrypted_storage(n1).
firewall(n1).

node(n2, amazon, 2, [ubuntu, sql], [python, rust, java, javascript], 0.001, us).
encrypted_storage(n2).
firewall(n2).

node(n3, amazon, 1, [ubuntu, sql], [python, rust, java, javascript], 0.001, eu).
encrypted_storage(n3).
firewall(n3).

node(n4, amazon, 2, [ubuntu, sql], [python, rust, java, javascript], 0.001, eu).
encrypted_storage(n4).
firewall(n4).

%link(l1, 1, [n1, n2]).
0.7::link(l1, 1, [n1, n2]).
0.3::link(l1, 3, [n1, n2]).
link(l2, 1, [n2, n3]).
link(l3, 1, [n1, n4]).
0.3::link(l4, 1, [n3, n4]).
0.7::link(l4, 3, [n3, n4]).
%link(l4, 1, [n3, n4]).