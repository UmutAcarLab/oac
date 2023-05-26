from qiskit import *
import sys
from math import pi
from itertools import accumulate
from qiskit.transpiler import Layout


filename = sys.argv[1]
print("file = ", filename)
circ = QuantumCircuit.from_qasm_file(filename)
print("parsing done")

basis_gates = ['x', 'h', 'rz', 'add', 'cx']

def shit_basis(circ):
  return transpile(circ, basis_gates=basis_gates)

def relabel_qubits (circ):
  def relabel_list (circ):
    registers = circ.qregs
    cnt = 0
    relabel = []
    for qr in registers:
      num_qr = qr.size
      for i in range(num_qr):
        relabel.append((qr[i], cnt + i))
      cnt = cnt + num_qr
    return relabel
  rl = relabel_list(circ)
  return transpile (circ, initial_layout=dict(rl), basis_gates=basis_gates)

def dump (circ, f):
  circ.qasm(filename=f)

dump(relabel_qubits(shit_basis(circ)), "out.qasm")
