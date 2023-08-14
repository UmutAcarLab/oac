import subprocess
from multiprocessing.pool import ThreadPool
import os
from os import listdir
import sys
from enum import Enum

def run_command(command):
	process = subprocess.Popen(command, shell=True)
	process.communicate()
	return command



def run_commands(commands, P):
	pool = ThreadPool(processes=P)
	results = pool.map(run_command, commands)
	pool.close()
	pool.join()
	return results

commands = ["./run_queso_normal.sh ham15-med.qasm.preprocessed.new 8 -removeSizePreservingRules > ham15-med.out", "./run_queso_normal.sh ham15-high.qasm.preprocessed.new 22 -removeSizePreservingRules > ham15-high.out", "./run_queso_normal.sh hhl_n7_from_python.qasm.preprocessed.new 10 -removeSizePreservingRules > hhl_n7_from_python.out", "./run_queso_normal.sh hhl_n9_from_python.qasm.preprocessed.new 130 -removeSizePreservingRules > hhl_n9_from_python.out", "./run_queso_normal.sh gf2^16_mult.qasm.preprocessed.new 8 -removeSizePreservingRules > gf2^16_mult.out", "./run_queso_normal.sh gf2^32_mult.qasm.preprocessed.new 137 -removeSizePreservingRules > gf2^32_mult.out", "./run_queso_normal.sh grover_n7_from_python.qasm.preprocessed.new 9 -removeSizePreservingRules > grover_n7_from_python.out", "./run_queso_normal.sh grover_n9_from_python.qasm.preprocessed.new 28 -removeSizePreservingRules > grover_n9_from_python.out", "./run_queso_normal.sh grover_n11_from_python.qasm.preprocessed.new 78 -removeSizePreservingRules > grover_n11_from_python.out", "./run_queso_normal.sh grover_n15_from_python.qasm.preprocessed.new 583 -removeSizePreservingRules > grover_n15_from_python.out", "./run_queso_normal.sh qft_n48_from_qiskit.qasm.preprocessed.new 15 -removeSizePreservingRules > qft_n48_from_qiskit.out", "./run_queso_normal.sh qft_n64_from_qiskit.qasm.preprocessed.new 31 -removeSizePreservingRules > qft_n64_from_qiskit.out", "./run_queso_normal.sh qft_n80_from_qiskit.qasm.preprocessed.new 46 -removeSizePreservingRules > qft_n80_from_qiskit.out", "./run_queso_normal.sh qft_n96_from_qiskit.qasm.preprocessed.new 66 -removeSizePreservingRules > qft_n96_from_qiskit.out", "./run_queso_normal.sh shor_7_mod_15_n8_from_python.qasm.preprocessed.new 4 -removeSizePreservingRules > shor_7_mod_15_n8_from_python.out", "./run_queso_normal.sh shor_7_mod_15_n10_from_python.qasm.preprocessed.new 17 -removeSizePreservingRules > shor_7_mod_15_n10_from_python.out", "./run_queso_normal.sh vqe_n12_from_python.qasm.preprocessed.new 37 -removeSizePreservingRules > vqe_n12_from_python.out", "./run_queso_normal.sh vqe_n16_from_python.qasm.preprocessed.new 87 -removeSizePreservingRules > vqe_n16_from_python.out", "./run_queso_normal.sh vqe_n20_from_python.qasm.preprocessed.new 166 -removeSizePreservingRules > vqe_n20_from_python.out", "./run_queso_normal.sh vqe_n24_from_python.qasm.preprocessed.new 316 -removeSizePreservingRules > vqe_n24_from_python.out"]
commands += ["./run_queso_normal.sh ham15-med.qasm.preprocessed.new 97 > t-mid/ham15-med.out",  "./run_queso_normal.sh ham15-high.qasm.preprocessed.new 384 > t-mid/ham15-high.out",  "./run_queso_normal.sh hhl_n7_from_python.qasm.preprocessed.new 1244 > t-mid/hhl_n7_from_python.out",  "./run_queso_normal.sh hhl_n9_from_python.qasm.preprocessed.new 10804 > t-mid/hhl_n9_from_python.out",  "./run_queso_normal.sh gf2^16_mult.qasm.preprocessed.new 285 > t-mid/gf2^16_mult.out",  "./run_queso_normal.sh gf2^32_mult.qasm.preprocessed.new 381 > t-mid/gf2^32_mult.out",  "./run_queso_normal.sh grover_n7_from_python.qasm.preprocessed.new 230 > t-mid/grover_n7_from_python.out",  "./run_queso_normal.sh grover_n9_from_python.qasm.preprocessed.new 840 > t-mid/grover_n9_from_python.out",  "./run_queso_normal.sh grover_n11_from_python.qasm.preprocessed.new 2713 > t-mid/grover_n11_from_python.out",  "./run_queso_normal.sh grover_n15_from_python.qasm.preprocessed.new 10800 > t-mid/grover_n15_from_python.out",  "./run_queso_normal.sh qft_n48_from_qiskit.qasm.preprocessed.new 2218 > t-mid/qft_n48_from_qiskit.out",  "./run_queso_normal.sh qft_n64_from_qiskit.qasm.preprocessed.new 3330 > t-mid/qft_n64_from_qiskit.out",  "./run_queso_normal.sh qft_n80_from_qiskit.qasm.preprocessed.new 4273 > t-mid/qft_n80_from_qiskit.out",  "./run_queso_normal.sh qft_n96_from_qiskit.qasm.preprocessed.new 5237 > t-mid/qft_n96_from_qiskit.out",  "./run_queso_normal.sh shor_7_mod_15_n8_from_python.qasm.preprocessed.new 242 > t-mid/shor_7_mod_15_n8_from_python.out",  "./run_queso_normal.sh shor_7_mod_15_n10_from_python.qasm.preprocessed.new 880 > t-mid/shor_7_mod_15_n10_from_python.out",  "./run_queso_normal.sh vqe_n12_from_python.qasm.preprocessed.new 620 > t-mid/vqe_n12_from_python.out",  "./run_queso_normal.sh vqe_n16_from_python.qasm.preprocessed.new 1285 > t-mid/vqe_n16_from_python.out",  "./run_queso_normal.sh vqe_n20_from_python.qasm.preprocessed.new 2003 > t-mid/vqe_n20_from_python.out",  "./run_queso_normal.sh vqe_n24_from_python.qasm.preprocessed.new 3278 > t-mid/vqe_n24_from_python.out"]
commands = commands[0:18]
print(commands)
P = 4  # Maximum parallel commands

if (len(sys.argv) < 2):
	print("no arguments\n")
	exit()
results = run_commands(commands, P)

