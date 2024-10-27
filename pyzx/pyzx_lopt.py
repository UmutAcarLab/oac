import pyzx as zx
import sys


def optimize(in_file, out_file):
    circuit = zx.Circuit.from_qasm_file(in_file)
    g = circuit.to_graph()
    zx.full_reduce(g)
    c_opt = zx.extract_circuit(g)
    qasm_opt = c_opt.to_qasm()
    with open(out_file, 'w') as f:
        f.write(qasm_opt)


if __name__ == '__main__':
    optimize(sys.argv[1], sys.argv[2])
