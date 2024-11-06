import os
from os import listdir
import re
import sys
from enum import Enum
from runutils import *

def pyzx(path, bench_name, cnot):
	cpath = path + bench_name
	suffix = ".pyzx"
	if cnot:
		suffix+=".cnot"
	options = {
			'timeout': 3600*3,
			'size': 6,
			'rl': '',
			'circuit': cpath  + ".qasm",
			'outfile' : cpath  + suffix + ".output.qasm",
			'logfile' : cpath  + suffix + ".log",
			'grain' : 100000000,
			'nopp': '',
			'runtool': '',
			'greedyonly': ''
	}
	command = './bin/ct2.mlton.bin'

	for option, value in options.items():
		if value:
				command += ' -{} {}'.format(option, value)
		else:
				command += ' -{}'.format(option)

	output_file = cpath + suffix + ".trash.out"
	command += ' > {}'.format(output_file)
	if cnot:
		command += ' -{}'.format('cnotmin')
	return command

def get_num_qubits(file):
	with open(file, 'r') as f:
		for line in f.readlines():
			if line.strip().startswith('qreg'):  # assume only one qreg
				return int(re.findall(r'\d+', line)[-1])
	return 0

def lopt_pyzx(path, bench_name, size, grain_per_qubit, cnot):
	cpath = path + bench_name
	n = get_num_qubits(cpath + ".qasm")
	suffix = ".lopt.pyzx.{}.{}.converge".format(size, grain_per_qubit)
	if cnot:
		suffix+=".cnot"
	options = {
			'timeout': 3600*3,
			'size': size,
			'grain': grain_per_qubit * n,
			'rl': '',
			'circuit': cpath + ".qasm",
			'outfile' : cpath + suffix + ".output.qasm",
			'logfile' : cpath + suffix + ".log",
			'nopp': '',
			'greedyonly': ''
	}
	command = './bin/ct2.mlton.bin'

	for option, value in options.items():
		if value:
				command += ' -{} {}'.format(option, value)
		else:
				command += ' -{}'.format(option)
	if cnot:
		command += ' -{}'.format('cnotmin')
	output_file = cpath + suffix + ".trash.out"
	command += ' > {}'.format(output_file)
	command += " ; rm -f /mnt/ramdisk/%s.*; rm -f /mnt/ramdisk/optimized_nam_%s.*; "%(bench_name, bench_name)

	return command

def lopt_pyzx_size (path, bench_name, suffix, size):
	cpath = path + bench_name
	suffix+=".lopt.pyzx.size.%d"%(size)
	options = {
			'timeout': 3600*3,
			'size': size,
			'rl': '',
			'circuit': cpath + ".qasm",
			'logfile' : cpath + suffix + ".log",
			'nopp': '',
			'greedyonly': ''
	}
	command = './bin/ct2.mlton.bin'

	for option, value in options.items():
		if value:
				command += ' -{} {}'.format(option, value)
		else:
				command += ' -{}'.format(option)

	output_file = cpath + suffix + ".lopt.trash.out"
	command += ' > {}'.format(output_file)
# command += ' > {}'.format(output_file)
	return command


def voqc(path, bench_name, suffix):
	cpath = path + bench_name
	suffix+=".voqc"
	circuit =  cpath + ".qasm"
	command = '/root/quicr/quicr/optimizer/lib/SQIR/VOQC/_build/default/voqc.exe'
	options = {
			'i' : circuit,
			'o' : cpath + suffix + ".output",
	}

	for option, value in options.items():
		if option == 'circuit':
				command += ' {}'.format(value)
		elif option == 'dump':
				continue
		elif value:
				command += ' -{} {}'.format(option, value)
		else:
				raise Exception("voqc doesn't have option less arguments")
	output_file = cpath + suffix + ".combined.log"
	command += ' > {}'.format(output_file)

	return command


def size_exp (bn, sizes):
	return list(map (lambda s: lopt_pyzx_size(path, bn, "", s), sizes))

def lopt_voqc (path, bench_name, suffix):
	cpath = path + bench_name
	suffix+=".lopt"
	wtcomb = "voqc"
	options = {
			'timeout': 3600*3,
			# 'wtcomb': wtcomb,
			'size': 40,
			'nopp': '',
			# 'rl': '',
			'greedyonly': '',
			'circuit' : cpath + ".qasm",
			'outfile' : cpath + suffix + "." + wtcomb + ".output",
			'logfile' : cpath + suffix + "." + wtcomb + ".log"
	}
	command = './bin/ct2.mlton.voqc.bin'

	for option, value in options.items():
		if value:
				command += ' -{} {}'.format(option, value)
		else:
				command += ' -{}'.format(option)

	output_file = cpath + "." + wtcomb + ".trash.out"
	command += ' > {}'.format(output_file)
# command += ' > {}'.format(output_file)

	return command


nwq_list = ["nwq_binary_welded_tree_n17",
"nwq_multiplier_n200",
"nwq_statevector_n4",
"nwq_binary_welded_tree_n21",
"nwq_multiplier_n300",
"nwq_statevector_n6",
"nwq_boolean_satisfaction_n24",
"nwq_multiplier_n400",
"nwq_vqc_n120",
"nwq_boolean_satisfaction_n28",
"nwq_square_root_n21",
"nwq_vqc_n15",
"nwq_boolean_satisfaction_n30",
"nwq_square_root_n30",
"nwq_vqc_n240",
"nwq_boolean_satisfaction_n32",
"nwq_square_root_n42",
"nwq_vqc_n30",
"nwq_boolean_satisfaction_n34",
"nwq_square_root_n54",
"nwq_vqc_n60",
"nwq_multiplier_n100",
"nwq_square_root_n60"
]

path = "feyn_benchmarks/"
files = [f for f in os.listdir(path) if os.path.isfile(os.path.join(path, f))]
bench_files = [f for f in files if (f.endswith('.qasm'))]
bench_files = [f for f in bench_files if (not('trash' in f))]
bench_files = [f for f in bench_files if (not('output' in f))]
bench_files = [f for f in bench_files if (not('vqc' in f))]
bench_files = list(set([f.split('.')[0] for f in bench_files]))
bench_files = [f for f in bench_files]
bench_files = sorted(bench_files)
print(bench_files)
# bench_files = ["grover_n15_from_python"]
# bench_files = ["qft_n96_from_qiskit"]
# bench_files = ["nwq_square_root_n30"]
# bench_files = ["grover_n15_from_pythonp"]
sizes = [2, 5, 15, 30, 60, 120, 240, 480, 960, 1920, 3840, 7680]

# bench_files = ["nwq_statevector_n4"]

# bench_files = ["grover_n9_from_python_ts2", "grover_n11_from_python_ts2", "grover_n13_from_python_ts2", "grover_n15_from_python_ts2"]
# bench_files = ["hhl_n9_from_python", "shor_7_mod_15_n16_from_python"]
# bench_files = ["hwb10"]
pyzx_commands = list(map(lambda x: pyzx(path, x, False), bench_files))
lopt_commands = list(map(lambda x: lopt_pyzx(path, x, 100, 400, False), bench_files))
voqc_commands = list(map (lambda x : voqc(path, x, ""), bench_files))
lopt_voqc_commands = list(map (lambda x : lopt_voqc(path, x, ""), bench_files))
size_commands = size_exp("grover_n15_from_python", sizes)
commands = pyzx_commands
# commands = commands[0]
print(commands[0])
print("num commands", len(commands))

if (len(sys.argv) < 2):
	print("no arguments\n")
	exit()

P = 2 # Maximum parallel commands

results = run_commands(commands, P)
print(results)
