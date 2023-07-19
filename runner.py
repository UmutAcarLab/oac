import subprocess
from multiprocessing.pool import ThreadPool
from os import listdir

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
			'outfile' : cpath + bench_name + suffix + ".output",
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
	command += ' > {}'.format(output_file)

	return command


def remove_vqe_bench(b):
	filtered = [s for s in b if not s.startswith('vqe')]
	return filtered

bench_list = listdir('benchmarks/')
bench_list.remove('make')
bench_list.remove('original')
bench_list = sorted(remove_vqe_bench(bench_list))
# print(bench_list)
# print(len(bench_list))

# adder_8 has compatiblity issues
# ['adder_8', 'barenco_tof_10', 'barenco_tof_3', 'barenco_tof_4']
# curr_list = bench_list[0:4]


# quartz crapped out on gf benchmarks (except 2^10, which is done)
# ['barenco_tof_5', 'csla_mux_3', 'csum_mux_9', 'gf2^10_mult', 'gf2^128_mult', 'gf2^131_mult', 'gf2^163_mult', 'gf2^16_mult', 'gf2^32_mult', 'gf2^4_mult', 'gf2^5_mult', 'gf2^64_mult', 'gf2^6_mult', 'gf2^7_mult', 'gf2^8_mult', 'gf2^9_mult']
# curr_list = bench_list[4:20]


# ['grover_5', 'grover_n15_from_python', 'grover_n21_from_python', 'grover_n3_from_python', 'grover_n5_from_python', 'grover_n9_from_python', 'ham15-high', 'ham15-low', 'ham15-med', 'hhl_n10', 'hhl_n11_from_python', 'hhl_n13_from_python']
curr_list = bench_list[20:32]
print(curr_list)


lopt_commands = list(map (lambda x : lopt("benchmarks/", x, ""), curr_list))
quartz_commands = list(map (lambda x : quartz("benchmarks/", x, ""), curr_list))
commands = lopt_commands
# print(commands)
# command_list = read_commands (f)
# print(command_list)

# print(lopt("benchmarks/", "adder_8", ""))

P = 4  # Maximum parallel commands

results = run_commands(commands, P)
print(results)
