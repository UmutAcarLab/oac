import pyzx as zx
import sys
import time


def optimize(in_file, out_file):
    # print('optimize:', in_file, out_file)
    circuit = zx.Circuit.from_qasm_file(in_file)
    t0 = time.time()
    g = circuit.to_graph()
    t1 = time.time()
    print('to_graph_time:', t1 - t0)
    zx.full_reduce(g)
    t2 = time.time()
    print('full_reduce_time:', t2 - t1)
    c_opt = zx.extract_circuit(g)
    t3 = time.time()
    print('extract_circuit_time:', t3 - t2)
    qasm_opt = c_opt.to_qasm()
    # print(f'optimized from {circuit.stats_dict(False)["tcount"]} to {c_opt.stats_dict(False)["tcount"]}')
    # print(qasm_opt)
    with open(out_file, 'w') as f:
        f.write(qasm_opt)


if __name__ == '__main__':
    optimize(sys.argv[1], sys.argv[2])
