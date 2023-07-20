import subprocess
from multiprocessing.pool import ThreadPool
from os import listdir
import sys

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

def lopt (path, bench_name, suffix):
	cpath = path + bench_name + "/"
	suffix+=".lopt"
	options = {
			'eqset': 'Nam_6_3_complete_ECC_set.json',
			'timeout': 3600,
			'wt': 10,
			'size': 6,
			'rl': '',
			'circuit': cpath + bench_name + ".qasm",
			'outfile' : cpath + bench_name + suffix + ".output",
			'logfile' : cpath + bench_name + suffix + ".log"
	}
	command = './bin/ct2.mlton.quartz.bin'

	for option, value in options.items():
		if value:
				command += ' -{} {}'.format(option, value)
		else:
				command += ' -{}'.format(option)

	output_file = cpath + bench_name + ".trash.out"
	command += ' > {}'.format(output_file)
# command += ' > {}'.format(output_file)

	return command

def quartz(path, bench_name, suffix):
	cpath = path + bench_name + "/"
	suffix+=".quartz"
	circuit =  cpath + bench_name + ".qasm"
	command = './lib/quartz/build/test_nam ' + circuit
	options = {
			'eqset': 'Nam_6_3_complete_ECC_set.json',
			'timeout': 3600,
			'outfile' : "dummy.out",
			# 'outfile' : cpath + bench_name + suffix + ".output",
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
	output_file = cpath + bench_name + suffix + ".combined.log"
	# command += ' > {}'.format(output_file)

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


curr_list = filter_bn(bench_list, 'qft')
curr_list += ["adder_8", "ham15-high", "ham15-med", "mod_adder_1024", "qcla_adder_10", "shor_7_mod_15_n18_from_python"]
# print(quartz("benchmarks/", "qft_n160", ""))
# print(lopt("benchmarks/", "qft_n160", ""))

if (len(sys.argv) < 2):
	print("no arguments\n")
	exit()

lopt_commands = list(map (lambda x : lopt("benchmarks/", x, ""), curr_list))
quartz_commands = list(map (lambda x : quartz("benchmarks/", x, ""), curr_list))
commands =  lopt_commands
# print(commands)
# command_list = read_commands (f)
# print(command_list)

# print(lopt("benchmarks/", "adder_8", ""))

P = 8  # Maximum parallel commands

results = run_commands(commands, P)
print(results)
