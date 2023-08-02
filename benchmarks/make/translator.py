from qiskit import *
import sys
from math import pi
from itertools import accumulate
from qiskit.transpiler import Layout
import subprocess
from qiskit.circuit import QuantumCircuit
from qiskit.converters import circuit_to_dag
from qiskit.transpiler import TransformationPass


target_basis = ['t', 'h', 's', 'cx']
parameterized = False

GRID_SYNTH_PATH='./gridsynth'

def shit_basis(circ, target_basis):
  return transpile(circ, basis_gates=target_basis)

class RZTranslator(TransformationPass):

  def run_gridsynth (self, thet):
    arg = "(" + str(thet) + ")"
    print(arg)
    command = [GRID_SYNTH_PATH, arg, "--phase"]
    try:
      # Run the command and capture the output
      output = subprocess.check_output(command, stderr=subprocess.STDOUT)
      # Decode the output from bytes to string
      output = output.decode('utf-8').strip()
      return output
    except subprocess.CalledProcessError as e:
        print(f"Error: {e.output.decode('utf-8').strip()}")

  def decomp_rz (self, thet, qidx):
    s = self.run_gridsynth (thet)
    circuit = QuantumCircuit(1)
    gate_mapping = {
      'S': circuit.s,
      'H': circuit.h,
      'T': circuit.t,
      'X': circuit.x
    }
    for gs in s:
      gate = gate_mapping[gs]
      gate (0)
    return circuit
  """A transpiler pass to replace RYY and RZZ gates with RXX gates."""
  def run(self, dag):
    """Run the pass."""
    # iterate over all operations
    for node in dag.op_nodes():
      # if we hit a RYY or RZZ gate replace it
      if node.op.name in ["rz"]:
          # get the rotation angle
        angle = node.op.params[0]
        subcircuit = self.decomp_rz(angle, 0)
        print(subcircuit)

        dag.substitute_node_with_dag(node, circuit_to_dag(subcircuit))
    return dag


# ct = RZTranslator()(circ)
# print(ct.qasm())
def relabel_qubits (circ, basis):
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
  return transpile (circ, initial_layout=dict(rl),  basis_gates = basis)

def dump (circ, f):
  circ.qasm(filename=f)


def dump_file_name (f):
  pref = '.'.join (f.split('.')[:-1])
  return pref + '.clifft.qasm'

filename = sys.argv[1]
print("file = ", filename)
circ = QuantumCircuit.from_qasm_file(filename)

# If the target basis does not have parameterized gates, then the input should only have RZ as the parameterized gate
if (not parameterized):
  circ = RZTranslator()(circ)

# print(circ)
fc = relabel_qubits(shit_basis(circ, target_basis), target_basis)
print(filename, dump_file_name(filename))
dump(fc, dump_file_name(filename))

