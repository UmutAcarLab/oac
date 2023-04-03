OPENQASM 2.0;
include "qelib1.inc";
qreg q[3];
s q[0];
s q[1];
s q[0];
s q[2];
cnot q[0], q[1];
cnot q[0], q[2];
h q[0];
h q[1];
h q[2];

