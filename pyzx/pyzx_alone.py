import pyzx as zx
import sys
import time


def optimize(in_file, out_file):
    circuit = zx.Circuit.from_qasm_file(in_file)

    st = time.time()

    g = circuit.to_graph()

    opt_st = time.time()
    zx.full_reduce(g)
    opt_ed = time.time()

    c_opt = zx.extract_circuit(g)
    qasm_opt = c_opt.to_qasm()

    ed = time.time()
    t = ed - st
    t_opt = opt_ed - opt_st  # only on ZX-graph

    with open(out_file, 'w') as f:
        f.write(qasm_opt)
    tcount = [circuit.stats_dict(False)["tcount"], c_opt.stats_dict(False)["tcount"]]
    gatecount = [circuit.stats_dict(False)["gates"], c_opt.stats_dict(False)["gates"]]
    print(in_file, "gatecount = ", *gatecount, "tcount = ", *tcount)
    print("t_opt = ", t_opt, "time = ", t)


if __name__ == '__main__':
    optimize(sys.argv[1], sys.argv[2])
