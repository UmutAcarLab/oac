import subprocess
from multiprocessing.pool import ThreadPool
import os
from os import listdir
import sys
from enum import Enum


class GateSet (Enum):
	nam = ""
	clifft = ".clifft"
	# def str(gs):
	# 	if gs == GateSet.nam:
	# 		return "nam"
	# 	else:
	# 		return "clif"

	def eqset(gs):
		if gs == GateSet.nam:
			return "Nam_6_3_complete_ECC_set.json"
		else:
			return "CT_6_3_complete_ECC_set.json"

	def exec(gs):
		if gs == GateSet.nam:
			return "test_nam"
		else:
			return "test_cliff"

	def need_pp (gs):
		if gs == GateSet.nam:
			return True
		else:
			return False

class Tool (Enum):
  quartz = 1
  lopt = 2
  def name (t):
    if t == Tool.quartz:
      return "quartz"
    else:
      return "lopt"

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

f = "run_nam_mpl_search.sh"
def read_commands (f):
  with open (f) as file:
    return file.read().split('\n')


def loptwtc_queso (path, bench_name, suffix, gate_set, wtcomb):
	cpath = path + bench_name + "/"
	suffix+=".lopt.queso"
	options = {
			'timeout': 3600,
			'wt': 40,
			'wtcomb': wtcomb,
			'size': 6,
			'rl': '',
			'nopp': '',
			'circuit': cpath + bench_name + gate_set.value + ".qasm" + ".preprocessed" + ".new",
			'outfile' : cpath + bench_name + gate_set.value + suffix + ".output",
			'logfile' : cpath + bench_name + gate_set.value + suffix + ".log"
	}
	command = './bin/ct2.mlton.queso.bin'

	for option, value in options.items():
		if value:
				command += ' -{} {}'.format(option, value)
		else:
				command += ' -{}'.format(option)

	output_file = cpath + bench_name + gate_set.value + ".queso.trash.out"
	command += ' > {}'.format(output_file)
# command += ' > {}'.format(output_file)

	return command

def loptwtc (path, bench_name, suffix, gate_set, wtcomb):
	cpath = path + bench_name + "/"
	eqset = GateSet.eqset(gate_set)
	suffix+=".lopt"
	options = {
			'eqset': eqset,
			'timeout': 3600*3,
			'wtcomb': wtcomb,
			'size': 6,
			'rl': '',
			'circuit': cpath + bench_name + gate_set.value + ".qasm" + ".preprocessed",
			'outfile' : cpath + bench_name + gate_set.value + suffix + "." + wtcomb + ".output",
			'logfile' : cpath + bench_name + gate_set.value + suffix + "." + wtcomb + ".log"
	}
	command = './bin/ct2.mlton.quartz.bin'

	for option, value in options.items():
		if value:
				command += ' -{} {}'.format(option, value)
		else:
				command += ' -{}'.format(option)

	output_file = cpath + bench_name + gate_set.value + "." + wtcomb + ".trash.out"
	command += ' > {}'.format(output_file)
# command += ' > {}'.format(output_file)

	return command

def quartz(path, bench_name, suffix, gate_set):
	cpath = path + bench_name + "/"
	suffix+=".quartz"
	circuit =  cpath + bench_name + gate_set.value + ".qasm" +  ".preprocessed"
	command = './lib/quartz/build/' + GateSet.exec(gate_set) + ' ' + circuit
	eqset = GateSet.eqset(gate_set)
	options = {
			'eqset': eqset,
			'timeout': 3600*3,
			'outfile' : cpath + bench_name + gate_set.value + suffix + ".output",
	}

	for option, value in options.items():
		if option == 'circuit':
				command += ' {}'.format(value)
		elif option == 'dump':
				continue
		elif value:
				command += ' --{} {}'.format(option, value)
		else:
				raise Exception("quartz doesn't have option less arguments")
	output_file = cpath + bench_name + gate_set.value + suffix + ".combined.log"
	command += ' > {}'.format(output_file)

	return command

def queso(path, bench_name, suffix, gate_set):
	cpath = path + bench_name + "/"
	suffix+=".queso"
	circuit =  cpath + bench_name + gate_set.value + ".qasm" +  ".preprocessed.new"
	quesoDir = "lib/queso/QUESO/"
	jarFile = quesoDir + "SymbolicOptimizer-1.0-SNAPSHOT-jar-with-dependencies.jar"
	command = 'java --enable-preview -cp ' + jarFile + ' Applier'
	eqset = GateSet.eqset(gate_set)
	options = {
			'g' : 'nam',
			'r' : quesoDir + 'rules_q3_s6_nam.txt',
			'sr' : quesoDir + 'rules_q3_s3_nam_symb.txt',
			't': 600,
			'c' : circuit,
			'o' : cpath + bench_name + gate_set.value + suffix + ".output",
	}

	for option, value in options.items():
		command += ' -{} {}'.format(option, value)
	output_file = cpath + bench_name + gate_set.value + suffix + ".combined.log"
	command += ' > {}'.format(output_file+".trash")
	command += ' 2>{}'.format(output_file)
	return command

def remove_vqe_bench(b):
	filtered = [s for s in b if not s.startswith('vqe')]
	return filtered

def filter_bn (b, bn):
	filtered = [s for s in b if s.startswith(bn)]
	return filtered

bench_list = sorted(listdir('benchmarks/'))
bench_list.remove('make')
bench_list.remove('original')
bench_list.remove('gf2^128_mult')
bench_list.remove('gf2^163_mult')
bench_list.remove('gf2^131_mult')
bench_list.remove('gf2^64_mult')


# bench_list = sorted(remove_vqe_bench(bench_list))
# print(bench_list)
# print(len(bench_list))

# ['hhl_n5_from_python', 'hhl_n7', 'hhl_n7_from_python', 'hhl_n9_from_python']
# curr_list = bench_list[32:36]

# mod_adder_1024 incompatible
# ['hwb6', 'mod5_4', 'mod_adder_1024', 'mod_mult_55', 'mod_red_21', 'multiplier_n45', 'multiplier_n75', 'qaoa_n24_from_python']
# curr_list = bench_list[36:44]


# qcla_adder_10 incompatible
# ['qaoa_n6', 'qaoa_n6_from_python', 'qaoa_n8_from_python', 'qcla_adder_10', 'qcla_com_7', 'qcla_mod_7', 'qft_n160', 'qft_n16_from_python']
# curr_list = bench_list[44:52]

# ['qft_n18', 'qft_n24_from_python', 'qft_n29', 'qft_n30_from_python', 'qft_n320', 'qft_n4_from_python', 'qft_n63', 'qft_n8_from_python', 'rc_adder_6', 'shor_7_mod_15_n12_from_python', 'shor_7_mod_15_n16_from_python', 'shor_7_mod_15_n18_from_python', 'shor_7_mod_15_n8_from_python', 'shor_n5', 'tof_10', 'tof_3', 'tof_4', 'tof_5', 'vbe_adder_3', 'vqe_n16_from_python', 'vqe_n24', 'vqe_n24_from_python', 'vqe_n28_from_python', 'vqe_n4_from_python', 'vqe_n6_from_python', 'vqe_n8_from_python', 'vqe_uccsd_n28', 'vqe_uccsd_n4', 'vqe_uccsd_n6', 'vqe_uccsd_n8']
# curr_list = bench_list[52:]
# print(len(curr_list))


# ['gf2^10_mult', 'gf2^128_mult', 'gf2^131_mult', 'gf2^163_mult', 'gf2^16_mult', 'gf2^32_mult', 'gf2^4_mult', 'gf2^5_mult', 'gf2^64_mult', 'gf2^6_mult', 'gf2^7_mult', 'gf2^8_mult', 'gf2^9_mult']
# need to investigate again
# curr_list = filter_bn(bench_list, 'gf')


# curr_list = filter_bn(bench_list, 'qft')
# curr_list = []
# curr_list += ["adder_8", "ham15-high", "ham15-med", "mod_adder_1024", "qcla_adder_10", "shor_7_mod_15_n18_from_python"]
# curr_list += ["adder_8", "ham15-high", "ham15-med", "mod_adder_1024", "shor_7_mod_15_n18_from_python"]
# print(quartz("benchmarks/", "qft_n160", ""))
# print(lopt("benchmarks/", "qft_n160", ""))


# CLIFF T
# curr_list = ['hhl_n5_from_python', 'multiplier_n45', 'multiplier_n75', 'qaoa_n6', 'qaoa_n6_from_python', 'qaoa_n8_from_python', 'qft_n16_from_python', 'qft_n18', 'qft_n24_from_python', 'qft_n29', 'qft_n30_from_python', 'qft_n4_from_python', 'qft_n8_from_python', 'shor_7_mod_15_n12_from_python', 'shor_7_mod_15_n16_from_python', 'shor_7_mod_15_n18_from_python', 'shor_7_mod_15_n8_from_python']
# bench_list = remove_vqe_bench(bench_list)

#barenco_tof_10
# curr_list = ['adder_8', 'barenco_tof_10', 'barenco_tof_3', 'barenco_tof_4', 'barenco_tof_5', 'csla_mux_3', 'csum_mux_9', 'gf2^10_mult', 'gf2^16_mult', 'gf2^32_mult', 'gf2^4_mult', 'gf2^5_mult', 'gf2^6_mult', 'gf2^7_mult', 'gf2^8_mult', 'gf2^9_mult', 'grover_5', 'grover_n15_from_python', 'grover_n3_from_python', 'grover_n5_from_python', 'grover_n9_from_python', 'ham15-high', 'ham15-low', 'ham15-med', 'hhl_n10', 'hhl_n7_from_python', 'hwb6', 'rc_adder_6']

# curr_list = ['hhl_n7', 'hhl_n9_from_python', 'mod5_4', 'mod_adder_1024', 'mod_mult_55', 'mod_red_21', 'qaoa_n24_from_python', 'qcla_adder_10', 'qcla_com_7', 'qcla_mod_7', 'qft_n160', 'qft_n320', 'qft_n63', 'tof_10', 'tof_3', 'tof_4', 'tof_5', 'vbe_adder_3']
# curr_list = filter_bn (bench_list, "vqe")
curr_list = ["qft_n48_from_qiskit", "qft_n64_from_qiskit", "qft_n80_from_qiskit", "qft_n96_from_qiskit"]
curr_list = ["qft_n48_from_qiskit", "qft_n64_from_qiskit", "qft_n80_from_qiskit"]
curr_list += ["hhl_n7_from_python"]
curr_list = ["qft_n48_from_qiskit"]
# curr_list += filter_bn (bench_list, "ham15")
# curr_list += filter_bn (bench_list, "grover")
# curr_list += filter_bn (bench_list, "qft")
# curr_list += filter_bn (bench_list, "qaoa")
# curr_list += filter_bn (bench_list, "shor")
# curr_list += filter_bn (bench_list, "hhl")
# print(loptwtc("benchmarks/", "grover_n9_from_python", "", GateSet.nam, "0.3"))
# curr_list = []
# curr_list += filter_bn (bench_list, "multiplier")
# curr_list += filter_bn (bench_list, "gf")
# curr_list += ['gf2^16_mult', 'gf2^32_mult']
# curr_list = ["vqe_n12_from_python", "vqe_n20_from_python"]

# curr_list = ["vqe_n12_from_python", "vqe_n8_from_python", "qft_n64_from_qiskit", "qft_n48_from_qiskit"]
# curr_list = ["hhl_n9_from_python"]
# curr_list = ["qaoa_n30_from_python"]
# curr_list = ["hhl_n7_from_python"]
print(curr_list)
lopt_commands = list(map (lambda x : loptwtc("benchmarks/", x, "", GateSet.nam, "0.001"), curr_list))
# curr_list = ['ham15-med', 'ham15-high', 'hhl_n7_from_python', 'hhl_n9_from_python', 'hhl_n11_from_python', 'gf2^16_mult', 'gf2^32_mult', 'grover_n7_from_python', 'grover_n9_from_python', 'grover_n11_from_python', 'grover_n15_from_python', 'qft_n48_from_qiskit', 'qft_n64_from_qiskit', 'qft_n80_from_qiskit', 'qft_n96_from_qiskit', 'shor_7_mod_15_n8_from_python', 'shor_7_mod_15_n10_from_python', 'shor_7_mod_15_n12_from_python', 'shor_7_mod_15_n14_from_python', 'vqe_n12_from_python', 'vqe_n16_from_python', 'vqe_n24_from_python', 'vqe_n20_from_python']
# lopt_commands = list(map (lambda x : loptwtc("benchmarks/", x, "", GateSet.nam, "0.1"), curr_list))
# lopt_commands = list(map (lambda x : lopt("benchmarks/", x, "", GateSet.nam), curr_list))

# curr_list = []
# curr_list += filter_bn (bench_list, "qaoa")
# curr_list += ["grover_n7_from_python", "grover_n11_from_python", "shor_7_mod_15_n10_from_python", "shor_7_mod_15_n14_from_python"]
# # # lopt_commands += list(map (lambda x : lopt("benchmarks/", x, "", GateSet.nam, "8"), curr_list))
# lopt_commands += list(map (lambda x : lopt("benchmarks/", x, "", GateSet.nam, "1"), curr_list))
# curr_list = ["grover_n11_from_python", "grover_n15_from_python", "grover_n21_from_python"]
# curr_list += ["grover_n9_from_python", "ham15-high", "hhl_n11_from_python", "hhl_n7_from_python"]
# curr_list += ["hhl_n9_from_python"]
# curr_list += filter_bn (bench_list, "shor")
# curr_list = filter_bn (bench_list, "vqe")
# curr_list += ["shor_7_mod_15_n16_from_python", "hhl_n13_from_python"]
# quartz_commands = []
# quartz_commands = list(map (lambda x : quartz("benchmarks/", x, "", GateSet.nam), curr_list))
# queso_commands = list(map (lambda x : queso("benchmarks/", x, "", GateSet.nam), curr_list))

commands = lopt_commands
print(commands)

if (len(sys.argv) < 2):
	print("no arguments\n")
	exit()
# command_list = read_commands (f)

# print(lopt("benchmarks/", "adder_8", ""))

P = 8  # Maximum parallel commands

results = run_commands(commands, P)
print(results)
