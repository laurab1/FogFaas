node(n1, amazon, 3, [ubuntu, sql], [python, rust, java, javascript], 0.001, eu).
encrypted_storage(n1).
firewall(n1).

node(n2, amazon, 2, [ubuntu, sql], [python, rust, java, javascript], 0.001, vat).
encrypted_storage(n2).
firewall(n2).

node(n3, amazon, 2, [ubuntu, sql], [python, rust, java, javascript], 0.001, eu).
encrypted_storage(n2).
firewall(n2).

link(l1, 1, n1, n2).
link(l2, 1, n2, n3).