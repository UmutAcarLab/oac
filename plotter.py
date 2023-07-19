import matplotlib.pyplot as plt
import glob
from os import listdir

BASE_DIR = "benchmarks/"

def parse_data (d):
  if d == "":
    return ([], [])
  d = d.replace('~', '-').replace('Ee', 'E')
  data_points = d.split(";")
  data_points.pop()
  time_values = []
  circuit_size_values = []
  for point in data_points:
    values = point.strip("()").split(", ")
    # Extracting time and circuit size
    if (float(values[0]) < 0):
      continue
    time_values.append(float(values[0]))
    circuit_size_values.append(int(float(values[1])))
  return (time_values, circuit_size_values)



def read_file(f):
  with open (f) as file:
    return file.read()

def read_quartz_log (f):
  d = read_file (f)
  lines = d.split('\n')
  greedy_points = lines[2].split(";")
  print(greedy_points)
  search_points = lines[-3].split(";")
  greedy_points.pop()
  search_points.pop()
  time_values = []
  circuit_size_values = []
  for point in greedy_points:
    values = point.strip("()").split(",")
    tv = float(values[0])
    time_values.append(tv)
    circuit_size_values.append(int(float(values[1])))

  gt = time_values[-1]
  for point in search_points:
    values = point.strip("()").split(",")
    tv = float(values[0])
    time_values.append(tv + gt)
    circuit_size_values.append(int(float(values[1])))
  return (time_values, circuit_size_values, gt)

def read_local_log (f):
  d = read_file(f)
  if d == "":
    return ([], [])
  d = d.replace('~', '-').replace('Ee', 'E')
  rows = d.split("\n")
  data_points = rows[0].split(";")
  data_points.pop()
  time_values = []
  circuit_size_values = []
  def parse_point (p):
    values = p.strip("()").split(",")
    return (float(values[0]), int(float(values[1])))
  for point in data_points:
    (tv, szv) = parse_point(point)
    time_values.append(tv)
    circuit_size_values.append(szv)
  gt = parse_point(rows[1].split(";")[0])[0]
  return (time_values, circuit_size_values, gt)

def log_from_bench(bn, quartz):
  if quartz:
    return read_quartz_log (BASE_DIR + bn + "/" + bn + ".quartz.combined.log")
  else:
    return read_local_log(BASE_DIR + bn + "/" + bn + ".lopt.log")

def plot_bench (bn, greedy):
  # if greedy:
  #   suffix = '.qasm.greedy.log'
  # else:
  #   suffix = '.qasm.search.log'
  (tq, szq, gtq) = log_from_bench(bn, True)
  print(tq, szq)
  if (len(tq) >= 2):
    plt.plot(tq, szq, marker = 's', label='Quartz', color = 'blue')
    plt.plot(tq[-1], szq[-1], marker = 's', color = 'blue')
    plt.axvline(x=gtq, color='red', linestyle='--')
  (tl, szl, gtl) = log_from_bench(bn, False)
  if (len(tl) >= 2):
    plt.plot(tl, szl, marker = 'o', label='Local optimizer', color = 'green')
    plt.axvline(x = gtl, color='red', linestyle='--')
  plt.xlabel('Time')
  plt.ylabel('Circuit Size')
  plt.legend()
  plt.title('%s: Circuit Size vs. Time'%(bn.capitalize()))
  plt.savefig("plots/%s.combined.png"%(bn), dpi=300)
  # if greedy:
  #   plt.title('%s: Circuit Size vs. Time (greedy)'%(bn.capitalize()))
  #   plt.savefig("plots/%s.greedy.png"%(bn), dpi=300)
  # else:
  #   plt.title('%s: Circuit Size vs. Time (search)'%(bn.capitalize()))
  #   plt.savefig("plots/%s.search.png"%(bn), dpi=300)

plot_bench ("barenco_tof_3", False)
# print(read_local_log("logs/lopt/adder_8.qasm.combined.log"))
# print(read_local_log("logs/lopt/adder_8.qasm.combined.log"))
# Sample data

# example inputs
# d = "logs/lopt/"
# suffix = '.qasm.greedy.log'
def log_files_and_names (d, suffix):
  pat = "*" + suffix
  files = glob.glob(d + pat)
  matches = []
  for f in files:
    i = f.find(d) + len(d)
    j = f.find(suffix)
    match = f[i:j]
    matches.append(match)
  return list(zip(files, matches))

def csv_printer (l):
  for li in l:
    s = ""
    for e in li:
      s+=str(e)
      s+=","
    print(s[:-1])

def parse_lopt (greedy):
  suffix = ""
  suffix = '.qasm.combined.log'
  files = log_files_and_names("logs/lopt/", suffix)
  def report_from_file (f, bn):
    (times, sizes) = read_local_log(f)
    return (bn, sizes[0], sizes[-1], times [-1])
  results = list(map (lambda f: report_from_file(f[0], f[1]), files))
  results = sorted(results, key=lambda x: x[0])
  csv_printer(results)

def parse_quartz (greedy):
  suffix = '.qasm.combined.log'
  def report_from_file (f, bn):
    (times, sizes) = read_quartz_log (f)
    if len(sizes) < 2:
      return (bn, None, None, None)
    return (bn, sizes[0], sizes[-1], times [-1])
  files = log_files_and_names("logs/quartz/", suffix)
  results = list(map (lambda f: report_from_file(f[0], f[1]), files))
  results = sorted(results, key=lambda x: x[0])
  csv_printer(results)

# parse_quartz(False)
# parse_lopt(False)
# print(read_quartz_log("logs/quartz/" + "grover_n21_from_python"+ ".qasm.combined.log"))
# Plotting the graph

def remove_bench(b, bn):
	filtered = [s for s in b if not s.startswith(bn)]
	return filtered

bench_list = listdir(BASE_DIR)
bench_list.remove('make')
bench_list.remove('original')
bench_list = sorted(remove_bench(bench_list, 'vqe'))



# curr_list = sorted(bench_list[0:4])


curr_list = bench_list [4:8]
print(curr_list)

quartz_logs = list (map(lambda x: log_from_bench(x, True), curr_list))
lopt_logs = list (map(lambda x: log_from_bench(x, False), curr_list))

def cols_from_log (l):
  (tvs, sizes, _) = l
  return (sizes[0], sizes[-1])

# print(log_from_bench ("gf2^32_mult", True))

# LATEX IT
for (q, l) in zip(quartz_logs, lopt_logs):
  (q0, qf) = cols_from_log(q)
  (l0, lf) = cols_from_log(l)
  print(l0, q0, lf, qf)
