import matplotlib.pyplot as plt
import glob
from tabulate import tabulate
from os import listdir
from enum import Enum
import qiskit_api as qisk


class Tool (Enum):
  quartz = 1
  lopt = 2

BASE_DIR = "benchmarks/"
class Colors:
  # Regular colors
  BLACK = "\033[30m"
  RED = "\033[31m"
  GREEN = "\033[32m"
  YELLOW = "\033[33m"
  BLUE = "\033[34m"
  MAGENTA = "\033[35m"
  CYAN = "\033[36m"
  WHITE = "\033[37m"
  RESET = "\033[0m"



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
  # print(greedy_points)
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
  # if (len(time_values) <= 0):
    # print("no times in ", f)
    # return ([0], [0], 0)
  (gt, gv) = (time_values[-1], circuit_size_values[-1])
  for point in search_points:
    values = point.strip("()").split(",")
    tv = float(values[0])
    time_values.append(tv + gt)
    circuit_size_values.append(int(float(values[1])))
  return (time_values, circuit_size_values, gt, gv)

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
  gv = parse_point(rows[1].split(";")[0])[1]
  return (time_values, circuit_size_values, gt, gv)

def log_from_bench(bn, tool):
  if tool == Tool.quartz:
    return read_quartz_log (BASE_DIR + bn + "/" + bn + ".quartz.combined.log")
  else:
    return read_local_log(BASE_DIR + bn + "/" + bn + ".lopt.log")


def split (times, sizes, time):
  logs = list(zip(times, sizes))
  # print(list(logs))
  before = [t for t in logs if t[0] <= time]
  after = [t for t in logs if t[0] >= time]
  ub = list(zip(*before))
  ua = list(zip(*after))
  return (list(ub[0]), list(ub[1]), list(ua[0]), list(ua[1]))

def plot_bench (bn):
  # if greedy:
  #   suffix = '.qasm.greedy.log'
  # else:
  #   suffix = '.qasm.search.log'
  (tq, szq, gtq, _) = log_from_bench(bn, Tool.quartz)
  (qtg, qsg, qts, qss) = split(tq, szq, gtq)

  fig, ax = plt.subplots()

  if (len(tq) >= 2):
    ax.plot(qtg + qts, qsg + qss, marker = 's', label='Quartz', color = 'blue')
    # ax.set_xscale('log')
    # plt.xscale('log')
    # plt.
    # plt.plot(tq[-1], szq[-1], marker = 's', color = 'blue')
    # plt.axvline(x=gtq, color='red', linestyle='--')
  (tl, szl, gtl, _) = log_from_bench(bn, Tool.lopt)
  if (len(tl) >= 2):
    plt.plot(tl, szl, marker = 'o', label='Local optimizer', color = 'green')
    plt.plot(tl[-1], szl[-1], marker = 'o', color = 'green')
    # plt.axvline(x = gtl, color='red', linestyle='--')
  ax.set_xlabel('Time')
  ax.set_ylabel('Circuit Size')
  ax.legend()
  ax.set_title('%s: Circuit Size vs. Time'%(bn.capitalize()))
  fig.tight_layout()
  plt.savefig("plots/%s.combined.png"%(bn), dpi=300)


# plot_bench ("qft_n160", False)
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

bench_list = sorted(listdir(BASE_DIR))
bench_list.remove('make')
bench_list.remove('original')
bench_list.remove('shor_n5')
bench_list = sorted(remove_bench(bench_list, 'gf'))
bench_list = sorted(remove_bench(bench_list, 'qft'))

skip_list = ["adder_8", "ham15-high", "ham15-med", "mod_adder_1024", "qcla_adder_10", "shor_7_mod_15_n18_from_python"]
bench_list = list(filter (lambda x : not(x in skip_list), bench_list))

# curr_list = sorted(bench_list[0:4])


curr_list = bench_list
# print(curr_list)
# exit()


def cols_from_log (l):
  (tvs, sizes, gt, gv) = l
  return (sizes[0], sizes[-1], gv)

# print(cols_from_log(log_from_bench ("adder_8", True)))
# print(cols_from_log(log_from_bench ("adder_8", False)))

def create_plots(curr_list):
  print("here")
  quartz_logs = list (map(lambda x: (x, log_from_bench(x, Tool.quartz)), curr_list))
  lopt_logs = list (map(lambda x: (x, log_from_bench(x, Tool.lopt)), curr_list))
  plot_bench ("shor_7_mod_15_n12_from_python")

def get_out_file (bn, tool):
  if tool == Tool.quartz:
    return BASE_DIR + bn + "/" + bn + ".quartz.output"
  else: 
    return BASE_DIR + bn + "/" + bn + ".lopt.output"

def get_bench_file (bn):
  return BASE_DIR + bn + "/" + bn + ".qasm"

def create_tables (curr_list, write):
  quartz_logs = list (map(lambda x: (x, log_from_bench(x, Tool.quartz)), curr_list))
  lopt_logs = list (map(lambda x: (x, log_from_bench(x, Tool.lopt)), curr_list))
  headers = ["Name", "Original", "QUARTZ", "LOPT", "Original CX", "QUARTZ CX", "LOPT CX"]
  tab = []
  # LATEX IT
  for ((bn, q), (bnd, l)) in zip(quartz_logs, lopt_logs):
    print("bench = ", bn)
    (q0, qf, gqf) = cols_from_log(q)
    (l0, lf, glf) = cols_from_log(l)
    qfile = get_out_file (bn, Tool.quartz)
    lfile = get_out_file (bn, Tool.lopt)
    assert(bn == bnd)
    im = (1 - (float(lf)/float(qf))) * 100
    if (q0 == l0):
      tab.append([bn, q0, qf, lf, qisk.get_cx_count_fast(get_bench_file(bn)+".preprocessed"), qisk.get_cx_count_fast(qfile), qisk.get_cx_count_fast(lfile)])
    else:
      print(Colors.RED, bn, Colors.RESET)

  tab_len = len(tab)
  if (len(tab) > 30 and write):
    h = int(tab_len/2)
    tab1 = tab[0:h]
    tab2 = tab[h:]
    ltab1 = tabulate(tab1, headers=headers, tablefmt="latex")
    ltab2 = tabulate(tab2, headers=headers, tablefmt="latex")
    with open("prelim1.tex", "w") as f:
      f.write(ltab1)
    with open("prelim2.tex", "w") as f:
      f.write(ltab2)
  else:
    print(tab)
    print("not the case yet")

create_tables(curr_list, True)
