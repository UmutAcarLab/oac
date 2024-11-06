from os import listdir
import sys
from enum import Enum
from runutils import *

class GateSet (Enum):
	nam = ""
	clifft = ".clifft"
	ibm = ".ibm"
	def str(gs):
		if gs == GateSet.nam:
			return "nam"
		elif gs == GateSet.ibm:
			return "ibm"
		else:
			return "clif"

	def eqset(gs):
		if gs == GateSet.nam:
			return "lib/quartz/Nam_6_3_complete_ECC_set.json"
		elif gs == GateSet.ibm:
			return "lib/quartz/IBM_4_3_complete_ECC_set.json"
		else:
			return "CT_6_3_complete_ECC_set.json"

	def exec(gs):
		if gs == GateSet.nam:
			return "test_nam"
		elif gs == GateSet.ibm:
			return "test_ibmq"
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


f = "run_nam_mpl_search.sh"
def read_commands (f):
  with open (f) as file:
    return file.read().split('\n')


def loptwtc_queso (path, bench_name, suffix, gate_set, wtcomb):
	cpath = path + bench_name + "/"
	suffix+=".lopt.queso"
	options = {
			'timeout': 10*3600,
			# 'wt': 1000000,
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

	output_file = cpath + bench_name + gate_set.value + suffix + ".trash.out"
	command += ' > {}'.format(output_file)
# command += ' > {}'.format(output_file)

	return command

def loptwtc_quartz (path, bench_name, suffix, gate_set, wtcomb):
	cpath = path + bench_name + "/"
	eqset = GateSet.eqset(gate_set)
	suffix+=".lopt"
	wtcomb = "greedy"
	options = {
			'eqset': eqset,
			'timeout': 3600*3,
			# 'wtcomb': wtcomb,
			'gatecount': '',
			'size': 6,
			# 'rl': '',
			'greedyonly': '',
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

def lopt_voqc (path, bench_name, suffix, gate_set, wtcomb):
	cpath = path + bench_name + "/"
	suffix+=".lopt.convergence"
	wtcomb = "voqc"
	options = {
			'timeout': 3600*3,
			# 'wtcomb': wtcomb,
			'size': 40,
			'nopp': '',
			# 'rl': '',
			'greedyonly': '',
			'circuit': cpath + bench_name + gate_set.value + ".qasm" + ".preprocessed",
			'outfile' : cpath + bench_name + gate_set.value + suffix + "." + wtcomb + ".output",
			'logfile' : cpath + bench_name + gate_set.value + suffix + "." + wtcomb + ".log"
	}
	command = './bin/ct2.mlton.voqc.bin'

	for option, value in options.items():
		if value:
				command += ' -{} {}'.format(option, value)
		else:
				command += ' -{}'.format(option)

	output_file = cpath + bench_name + gate_set.value + ".convergence" + "." + wtcomb + ".trash.out"
	command += ' > {}'.format(output_file)
# command += ' > {}'.format(output_file)

	return command


def lopt_pyzx (path, bench_name, suffix, gate_set, wtcomb):
	cpath = path + bench_name + "/"
	suffix+=".lopt.convergence"
	wtcomb = "pyzx"
	options = {
		'timeout': 3600*3,
		# 'wtcomb': wtcomb,
		'size': 100,
		'nopp': '',
		# 'rl': '',
		'greedyonly': '',
		'circuit': cpath + bench_name + gate_set.value + ".qasm" # + ".preprocessed",
														 ,
		'outfile' : cpath + bench_name + gate_set.value + suffix + "." + wtcomb + ".output",
		'logfile' : cpath + bench_name + gate_set.value + suffix + "." + wtcomb + ".log"
	}
	command = './bin/ct2.mlton.pyzx.bin'

	for option, value in options.items():
		if value:
			command += ' -{} {}'.format(option, value)
		else:
			command += ' -{}'.format(option)

	output_file = cpath + bench_name + gate_set.value + ".convergence" + "." + wtcomb + ".trash.out"
	command += ' > {}'.format(output_file)
	# command += ' > {}'.format(output_file)

	return command

def lopt_voqc_size (path, bench_name, suffix, size):
	cpath = path + bench_name + "/"
	suffix+=".lopt.size.%d"%(size)
	wtcomb = "voqc"
	options = {
			'timeout': 3600*3,
			# 'wtcomb': wtcomb,
			'size': size,
			'nopp': '',
			# 'rl': '',
			'greedyonly': '',
			'circuit': cpath + bench_name + ".qasm" + ".preprocessed",
			'logfile' : cpath + bench_name + suffix + "." + wtcomb + ".log"
	}
	command = './bin/ct2.mlton.voqc.bin'

	for option, value in options.items():
		if value:
				command += ' -{} {}'.format(option, value)
		else:
				command += ' -{}'.format(option)

	output_file = cpath + bench_name + suffix + "." + wtcomb + ".trash.out"
	command += ' > {}'.format(output_file)
# command += ' > {}'.format(output_file)

	return command


def lopt_pyzx_size (path, bench_name, suffix, gate_set, size):
	cpath = path + bench_name + "/"
	suffix+=".lopt.size.%d"%(size)
	wtcomb = "pyzx"
	options = {
		'timeout': 3600*3,
		# 'wtcomb': wtcomb,
		'size': size,
		'nopp': '',
		# 'rl': '',
		'greedyonly': '',
		'circuit': cpath + bench_name + gate_set.value + ".qasm" # + ".preprocessed"
		,
		'logfile' : cpath + bench_name + gate_set.value + suffix + "." + wtcomb + ".log"
	}
	command = './bin/ct2.mlton.pyzx.bin'

	for option, value in options.items():
		if value:
			command += ' -{} {}'.format(option, value)
		else:
			command += ' -{}'.format(option)

	output_file = cpath + bench_name + suffix + "." + wtcomb + ".trash.out"
	command += ' > {}'.format(output_file)
	# command += ' > {}'.format(output_file)

	return command



def loptwtcsize (path, bench_name, suffix, gate_set, wtcomb, size):
	cpath = path + bench_name + "/"
	eqset = GateSet.eqset(gate_set)
	suffix+=".lopt.queso.%d"%(size)
	options = {
			'eqset': eqset,
			'timeout': 3600*3,
			'wtcomb': wtcomb,
			'size': size,
			'nopp': '',
			'rl': '',
			'circuit': cpath + bench_name + gate_set.value + ".qasm" + ".preprocessed.new",
			'outfile' : cpath + bench_name + gate_set.value + suffix + ".output",
			'logfile' : cpath + bench_name + gate_set.value + suffix + ".log"
	}
	command = './bin/ct2.mlton.queso.bin'

	for option, value in options.items():
		if value:
				command += ' -{} {}'.format(option, value)
		else:
				command += ' -{}'.format(option)

	output_file = cpath + bench_name + gate_set.value + suffix  + "." + wtcomb + ".trash.out"
	command += ' > {}'.format(output_file)
# command += ' > {}'.format(output_file)

	return command

def size_exp (bn, sizes):
	return list(map (lambda s: lopt_pyzx_size("benchmarks/", bn, "", GateSet.clifft, s), sizes))

def quartz(path, bench_name, suffix, gate_set):
	cpath = path + bench_name + "/"
	suffix+=".quartz.12"
	circuit =  cpath + bench_name + gate_set.value + ".qasm" +  ".preprocessed"
	command = './lib/quartz/build/' + GateSet.exec(gate_set) + ' ' + circuit
	eqset = GateSet.eqset(gate_set)
	options = {
			'eqset': eqset,
			'timeout': 3600*12,
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

def voqc(path, bench_name, suffix, gate_set):
	cpath = path + bench_name + "/"
	suffix+=".voqc"
	circuit =  cpath + bench_name + gate_set.value + ".qasm" +  ".preprocessed"
	command = './lib/mlvoqc/_build/default/example.exe'
	eqset = GateSet.eqset(gate_set)
	options = {
			'f' : circuit,
			'o' : cpath + bench_name + gate_set.value + suffix + ".output",
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
	output_file = cpath + bench_name + gate_set.value + suffix + ".combined.log"
	command += ' > {}'.format(output_file)

	return command


def pyzx(path, bench_name, suffix, gate_set):
	cpath = path + bench_name + "/"
	suffix+=".pyzx"
	circuit =  cpath + bench_name + gate_set.value + ".qasm" # +  ".preprocessed"
	command = 'timeout 12h python3 pyzx/pyzx_alone.py'
	options = {
		'circuit' : circuit,
		'o' : cpath + bench_name + gate_set.value + suffix + ".output",
	}

	for option, value in options.items():
		if option == 'circuit' or option == 'o':
			command += ' {}'.format(value)
		elif option == 'dump':
			continue
		elif value:
			command += ' -{} {}'.format(option, value)
		else:
			raise Exception("pyzx doesn't have option less arguments")
	output_file = cpath + bench_name + gate_set.value + suffix + ".combined.log"
	command += ' > {}'.format(output_file)

	return command


def queso(path, bench_name, suffix, gate_set):
	cpath = path + bench_name + "/"
	suffix+=".queso"
	circuit =  cpath + bench_name + gate_set.value + ".qasm" +  ".preprocessed.new"
	quesoDir = "lib/queso/"
	jarFile = quesoDir + "OG_QUESO/SymbolicOptimizer-1.0-SNAPSHOT-jar-with-dependencies.jar"
	command = 'java --enable-preview -cp ' + jarFile + ' Applier'
	eqset = GateSet.eqset(gate_set)
	if gate_set == GateSet.nam:
		rules = 'rules_q3_s6_nam.txt'
		srules = 'rules_q3_s3_nam_symb.txt'
	elif gate_set == GateSet.ibm:
		rules = 'rules_q3_s4_ibm.txt'
		srules = 'rules_q3_s4_ibm_symb.txt'
	options = {
			'g' : GateSet.str(gate_set),
			'r' : quesoDir + rules,
			'sr' : quesoDir + srules,
			't': 12*3600,
			'c' : circuit,
			'o' : cpath + bench_name + gate_set.value + suffix + ".output",
	}

	for option, value in options.items():
		command += ' -{} {}'.format(option, value)
	output_file = cpath + bench_name + gate_set.value + suffix + ".12.combined.log"
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

# QUESO RUNS
# curr_list = ["qft_n48_from_qiskit", "qft_n64_from_qiskit", "qft_n80_from_qiskit"]
# curr_list += ["hhl_n7_from_python"]
# curr_list += ["ham15-high"]

# curr_list = ["ham15-med", "hhl_n9_from_python", "grover_n7_from_python", "grover_n9_from_python", "vqe_n12_from_python"]
# curr_list = ["grover_n9_from_python", "hhl_n9_from_python"]

# "hhl_n9_from_python",
# curr_list = ["qft_n48_from_qiskit", "qft_n64_from_qiskit", "qft_n80_from_qiskit", "qft_n96_from_qiskit"]

# curr_list = ["hhl_n7_from_python", "ham15-med", "ham15-high", "gf2^16_mult", "gf2^32_mult", "grover_n9_from_python",  "grover_n7_from_python", "qft_n48_from_qiskit"]
curr_list = ["hhl_n11_from_python"]
# curr_list+= ["hhl_n7_from_python", "hhl_n9_from_python", "hhl_n11_from_python", "ham15-high", "ham15-med"]
# curr_list+= ["vqe_n12_from_python", "vqe_n16_from_python", "vqe_n20_from_python", "vqe_n24_from_python", "gf2^16_mult", "gf2^32_mult"]
# curr_list+= ["grover_n7_from_python", "grover_n9_from_python", "grover_n11_from_python", "grover_n15_from_python"]
# curr_list+= ["qft_n48_from_qiskit", "qft_n64_from_qiskit", "qft_n80_from_qiskit", "qft_n96_from_qiskit"]

# curr_list = ["hhl_n9_from_python", "hhl_n11_from_python", "grover_n15_from_python", "shor_7_mod_15_n10_from_python"]
# curr_list = ["shor_7_mod_15_n8_from_python"]

# curr_list = ["hhl_n7_from_python"]
# curr_list = ["ham15-med", "grover_n15_from_python", "hhl_n9_from_python", "hhl_n11_from_python", "shor_7_mod_15_n8_from_python", "shor_7_mod_15_n10_from_python"]
# curr_list += []
# curr_list += ["", "grover_n11_from_python"]

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
# curr_list = ['ham15-med', 'ham15-high', 'hhl_n7_from_python', 'hhl_n9_from_python', 'hhl_n11_from_python', 'gf2^16_mult', 'gf2^32_mult', 'grover_n7_from_python', 'grover_n9_from_python', 'grover_n11_from_python', 'grover_n15_from_python', 'qft_n48_from_qiskit', 'qft_n64_from_qiskit', 'qft_n80_from_qiskit', 'qft_n96_from_qiskit', 'shor_7_mod_15_n8_from_python', 'shor_7_mod_15_n10_from_python', 'shor_7_mod_15_n12_from_python', 'shor_7_mod_15_n14_from_python', 'vqe_n12_from_python', 'vqe_n16_from_python', 'vqe_n24_from_python', 'vqe_n20_from_python']
# TODO: QUARTZ RERUN
curr_list = ['nwq_square_root_n42', 'nwq_square_root_n48', 'shor_7_mod_15_n12_from_python', 'shor_7_mod_15_n16_from_python', 'nwq_binary_welded_tree_n17', 'nwq_statevector_n5', 'hhl_n9_from_python', 'vqe_n16_from_python', 'shor_7_mod_15_n14_from_python', 'grover_n13_from_python', 'vqe_n24_from_python', 'grover_n15_from_python', 'nwq_boolean_satisfaction_n28', 'nwq_square_root_n48', 'vqe_n20_from_python']
# curr_list = ['shor_7_mod_15_n12_from_python', 'shor_7_mod_15_n16_from_python', 'shor_7_mod_15_n14_from_python', 'grover_n13_from_python']

curr_list = ['nwq_square_root_n48', 'nwq_statevector_n5']

curr_list = ["hhl_n7_from_python",
	"hhl_n9_from_python",
	"hhl_n11_from_python",
	"grover_n9_from_python",
	"grover_n11_from_python",
	"grover_n13_from_python",
	"grover_n15_from_python",
	"shor_7_mod_15_n10_from_python",
	"shor_7_mod_15_n12_from_python",
	"shor_7_mod_15_n14_from_python",
	"shor_7_mod_15_n16_from_python",
	"vqe_n12_from_python",
	"vqe_n16_from_python",
	"vqe_n20_from_python",
	"vqe_n24_from_python",
	"nwq_binary_welded_tree_n17",
	"nwq_binary_welded_tree_n21",
	"nwq_boolean_satisfaction_n28",
	"nwq_boolean_satisfaction_n30",
	"nwq_boolean_satisfaction_n32",
	"nwq_boolean_satisfaction_n34",
	"nwq_square_root_n42",
	"nwq_square_root_n48",
	"nwq_square_root_n54",
	"nwq_square_root_n60",
	"nwq_statevector_n5",
	"nwq_statevector_n6",
	"nwq_statevector_n7",
	"nwq_statevector_n8"
]

# nwq_list = [
# 	"hhl_n7_from_python",
# 	"nwq_binary_welded_tree_n17",
# 	"hhl_n11_from_python",
# 	"nwq_statevector_n4",
# 	"nwq_binary_welded_tree_n21",
# 	"nwq_statevector_n6",
# 	"nwq_boolean_satisfaction_n24",
# 	"nwq_vqc_n15",
# 	"nwq_boolean_satisfaction_n28",
# 	"nwq_vqc_n30",
# 	"nwq_boolean_satisfaction_n34",
# 	"nwq_vqc_n60"
# ]

# voqc list
# nwq_list = [
# 	"nwq_boolean_satisfaction_n30",
# 	"nwq_square_root_n21",
# 	"nwq_square_root_n60",
# 	"nwq_statevector_n8",
# 	"nwq_binary_welded_tree_n21",
# 	"nwq_boolean_satisfaction_n32",
# 	"nwq_square_root_n30",
# 	"nwq_vqc_n120",
# 	"nwq_boolean_satisfaction_n34",
# 	"nwq_square_root_n42",
# 	"nwq_square_root_n54",
# 	"nwq_statevector_n7",
# 	"nwq_vqc_n240",
# 	"hhl_n11_from_python"
# ]

# nwq_list = [
# 	"nwq_boolean_satisfaction_n30",
# 	"nwq_square_root_n21",
# 	"nwq_square_root_n60",
# 	"nwq_statevector_n8",
# 	"nwq_boolean_satisfaction_n32",
# 	"nwq_square_root_n30",
# 	"nwq_vqc_n120",
# 	"nwq_square_root_n42",
# 	"nwq_square_root_n54",
# 	"nwq_statevector_n7",
# 	"nwq_vqc_n240",
# ]
# nwq_list = ["nwq_multiplier_n100", "nwq_multiplier_n200", "nwq_multiplier_n300", "nwq_multiplier_n400", "nwq_multiplier_n50"]
# curr_list = nwq_list

# TODO: BOTH
# curr_list = ["nwq_square_root_n48", "nwq_statevector_n5", "vqe_n28_from_python"]

# TODO: LOPT
# analyse why multipliet, and vqc don't work as well.

# TODO: VOQC
# curr_list = ["nwq_statevector_n7", "nwq_statevector_n8", "hhl_n11_from_python", "shor_7_mod_15_n16_from_python"]

#print(curr_list)
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
# curr_list = [
# 	"hhl_n9_from_python",
# 	"grover_n9_from_python",
# 	"grover_n11_from_python",
# 	"grover_n15_from_python",
# 	"shor_7_mod_15_n10_from_python",
# 	"vqe_n12_from_python",
# 	"vqe_n16_from_python",
# 	"vqe_n20_from_python",
# 	"vqe_n24_from_python"]
curr_list =  ["nwq_binary_welded_tree_n25", "nwq_binary_welded_tree_n29", "hhl_n13_from_python"]
curr_list = [
	"mod5_4",
	"adder_8",
	"nwq_boolean_satisfaction_n24",
	"nwq_boolean_satisfaction_n28",
	"nwq_binary_welded_tree_n17",
	"grover_n9_from_python",
	"grover_n15_from_python",
	"hhl_n7_from_python",
	"hhl_n9_from_python",
	"qft_n24_from_python",
	"qft_n30_from_python",
	"shor_7_mod_15_n12_from_python",
	"vqe_n8_from_python",
	"vqe_n16_from_python",
	"nwq_square_root_n42",
	"nwq_statevector_n4",
	"nwq_statevector_n6"
]
quartz_commands = list(map (lambda x : quartz("benchmarks/", x, "", GateSet.nam), curr_list))
queso_commands = list(map (lambda x : queso("benchmarks/", x, "", GateSet.nam), curr_list))
voqc_commands = list(map (lambda x : voqc("benchmarks/", x, "", GateSet.nam), curr_list))

lopt_commands = list(map (lambda x : lopt_pyzx("benchmarks/", x, "", GateSet.clifft, "0.01"), curr_list))
pyzx_commands = list(map (lambda x : pyzx("benchmarks/", x, "", GateSet.clifft), curr_list))

# lopt_commands =  list(map (lambda x : loptwtc_queso("benchmarks/", x, "", GateSet.nam), curr_list))
# size_commands = size_exp("hhl_n7_from_python", [1, 2, 4, 8, 16, 32, 64])

commands = pyzx_commands
sizes = [2, 5, 10, 20, 80, 160, 320, 640, 1280, 2560, 5120]
# commands = size_exp ("nwq_square_root_n42", sizes)

print(commands[0])
print("num commands = ", len(commands))

if (len(sys.argv) < 2):
	print("no arguments\n")
	exit()
# command_list = read_commands (f)

# print(lopt("benchmarks/", "adder_8", ""))


P = 2  # Maximum parallel commands
results = run_commands(commands, P)
print(results)
