import matplotlib.pyplot as plt
import glob
import os
from tabulate import tabulate
from os import listdir
from enum import Enum
import qiskit_api as qisk
from scipy.stats import linregress
from scipy.interpolate import interp1d
import numpy as np
from numpy.polynomial.polynomial import polyfit


from matplotlib.ticker import MultipleLocator

class GateSet (Enum):
  nam = ""
  clifft = ".clifft"

class Tool (Enum):
  quartz = 1
  lopt = 2
  def name (t):
    if t == Tool.quartz:
      return "quartz"
    else:
      return "lopt"

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

nam_basis = ['x', 'h', 'rz', 'add', 'cx']

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

def log_from_bench(bn, tool, gate_set):
  print("bench", bn)
  path = BASE_DIR + bn + "/" + bn + gate_set.value
  if tool == Tool.quartz:
    return read_quartz_log (path +  ".quartz.combined.log")
  else:
    return read_local_log(path +  ".lopt.log")


def split (times, sizes, time):
  logs = list(zip(times, sizes))
  # print(list(logs))
  before = [t for t in logs if t[0] <= time]
  after = [t for t in logs if t[0] >= time]
  ub = list(zip(*before))
  ua = list(zip(*after))
  return (list(ub[0]), list(ub[1]), list(ua[0]), list(ua[1]))

def plot_bench (bn, gate_set):
  # if greedy:
  #   suffix = '.qasm.greedy.log'
  # else:
  #   suffix = '.qasm.search.log'
  (tq, szq, gtq, _) = log_from_bench(bn, Tool.quartz, gate_set)
  (qtg, qsg, qts, qss) = split(tq, szq, gtq)

  fig, ax = plt.subplots()

  if (len(tq) >= 2):
    ax.plot(qtg + qts, qsg + qss, marker = 's', label='Quartz', color = 'blue')
    # ax.set_xscale('log')
    # plt.xscale('log')
    # plt.
    # plt.plot(tq[-1], szq[-1], marker = 's', color = 'blue')
    # plt.axvline(x=gtq, color='red', linestyle='--')
  (tl, szl, gtl, _) = log_from_bench(bn, Tool.lopt, gate_set)
  if (len(tl) >= 2):
    plt.plot(tl, szl, marker = 'o', label='Local optimizer', color = 'green')
    plt.plot(tl[-1], szl[-1], marker = 'o', color = 'green')
    # plt.axvline(x = gtl, color='red', linestyle='--')
  ax.set_xlabel('Time')
  ax.set_ylabel('Circuit Size')
  ax.legend()
  ax.set_title('%s: Circuit Size vs. Time'%(bn.capitalize()))
  fig.tight_layout()
  plt.savefig("plots/%s%s.combined.png"%(bn, gate_set.value()))

def plot_bench_inset (bn, gate_set):
  # if greedy:
  #   suffix = '.qasm.greedy.log'
  # else:
  #   suffix = '.qasm.search.log'
  (tq, szq, gtq, gsq) = log_from_bench(bn, Tool.quartz, gate_set)
  (qtg, qsg, qts, qss) = split(tq, szq, gtq)

  (tl, szl, gtl, gsl) = log_from_bench(bn, Tool.lopt, gate_set)
  (ltg, lsg, _, lss) = split(tl, szl, gtl)

  fig, ax = plt.subplots()

  inset = True
  if (gsq == szq[0] and gsl == szl[0]):
    inset = False

  if (inset):
    ax_inset = ax.inset_axes([0.5, 0.5, 0.5, 0.5])  # (x, y, width, height)
  # Plot the greedy phase on the inset axis



  if (len(tq) >= 2):
    ax.plot(qtg + qts, qsg + qss, marker = 's', label='Quartz', color = 'blue')
    if inset:
      ax_inset.plot(qtg, qsg,  marker = 's',label='Quartz', color='blue')
    # ax.set_xscale('log')
    # plt.xscale('log')
    # plt.
    # plt.plot(tq[-1], szq[-1], marker = 's', color = 'blue')
    # plt.axvline(x=gtq, color='red', linestyle='--')
  if (len(tl) >= 2):
    plt.plot(tl, szl, marker = 'o', label='Local optimizer', color = 'green')
    plt.plot(tl[-1], szl[-1], marker = 'o', color = 'green')
    print(ltg, lsg, gtl, tl)
    if inset:
      ax_inset.plot(ltg + [gtl], lsg + [gsl], label='Local optimizer', color='green',  marker = 'o')
    # plt.axvline(x = gtl, color='red', linestyle='--')
  # ax_inset.set_title('Zoomed Greedy Phase')
  ax.set_xlabel('Time')
  ax.set_ylabel('Circuit Size')
  if inset:
    ax_inset.legend(loc='upper right')
    ax.indicate_inset_zoom(ax_inset)
  ax.set_title('%s: Circuit Size vs. Time'%(bn.capitalize()))
  fig.tight_layout()
  plt.savefig("plots/%s%s.inset.png"%(bn, gate_set.value), dpi=300)
  plt.close()

def plot_scatter (curr_list, gate_set, fm):
  qlogs = list(map (lambda x: log_from_bench(x, Tool.quartz, gate_set), curr_list))
  yq = list(map (lambda x: x[2], qlogs))

  llogs = list(map (lambda x: log_from_bench(x, Tool.lopt, gate_set), curr_list))
  yl = list(map (lambda x: x[2], llogs))
  x =  list(map (lambda x: x[1][0], llogs))

  so = list(zip (x, yl, yq))
  so2 = sorted(so, key=lambda x : x[0])
  (x, yl, yq) = zip(*so2)

  # coefficients = np.polyfit(x, yq, 2)
  # xq_new = np.linspace(min(x), max(x), 10000)
  # yq_new = np.polyval(coefficients, xq_new)
  # plt.plot(xq_new, yq_new)
  # print(xq_new, yq_new)

  slope, intercept, qr_value, p_value, std_err = linregress(x, yq)
  slope, intercept, lr_value, p_value, std_err = linregress(x, yl)
  # fitted_values = [slope * x + intercept for x in y]



  # Create a scatter plot
  plt.plot(x, yq, label='quartz', marker='s', color = 'blue')
  plt.plot(x, yl, label='lopt', marker='o', color = 'green')
  # plt.xscale('log')
  if yq[-1] >= 3000:
    plt.axhline(y=3600, color='red', linestyle='-', label="T.O.")
  # plt.xscale('log')
  # plt.plot(y, fitted_values, color='red', label='Fitted Line')
  # Set plot title and labels
  plt.title("Time for greedy optimizations (%s)"%fm)
  plt.ylabel("Time")
  plt.xlabel("Circuit size")
  # plt.xlim(left = 0, right=1000)
  # plt.ylim(bottom =0, top=50)
  # plt.xpltis.set_major_locator(MultipleLocator(1000))
  plt.legend()
  # plt.grid(True)

  # for x, y, fitted_y in zip(y, greedy_times, fitted_values):
  #   plt.plot([x, x], [y, fitted_y], color='gray', linestyle='dotted')


  # Show the plot
  plt.savefig("plots/%s.greedyscatter%s.png"%(fm, gate_set.value), dpi=300)
  plt.close()
  qr_squared = qr_value ** 2
  lr_squared = lr_value ** 2
  return (qr_squared, lr_squared)



def plot_size (curr_list, gate_set, fm):
  qlogs = list(map (lambda x: log_from_bench(x, Tool.quartz, gate_set), curr_list))
  yq = list(map (lambda x: x[1][-1], qlogs))

  llogs = list(map (lambda x: log_from_bench(x, Tool.lopt, gate_set), curr_list))
  yl = list(map (lambda x: x[1][-1], llogs))
  x =  list(map (lambda x: x[1][0], llogs))

  so = list(zip (x, yl, yq))
  so2 = sorted(so, key=lambda x : x[0])
  (x, yl, yq) = zip(*so2)

  # coefficients = np.polyfit(x, yq, 2)
  # xq_new = np.linspace(min(x), max(x), 10000)
  # yq_new = np.polyval(coefficients, xq_new)
  # plt.plot(xq_new, yq_new)
  # print(xq_new, yq_new)

  slope, intercept, qr_value, p_value, std_err = linregress(x, yq)
  slope, intercept, lr_value, p_value, std_err = linregress(x, yl)
  # fitted_values = [slope * x + intercept for x in y]



  # Create a scatter plot
  plt.plot(x, yq, label='quartz', marker='s', color = 'blue')
  plt.plot(x, yl, label='lopt', marker='o', color = 'green')
  # plt.xscale('log')
  # plt.xscale('log')
  # plt.plot(y, fitted_values, color='red', label='Fitted Line')
  # Set plot title and labels
  plt.title("End to end optimization (%s)"%fm)
  plt.ylabel("Post optimization size")
  plt.xlabel("Circuit size")
  # plt.xlim(left = 0, right=1000)
  # plt.ylim(bottom =0, top=50)
  # plt.xpltis.set_major_locator(MultipleLocator(1000))
  plt.legend()
  # plt.grid(True)

  # for x, y, fitted_y in zip(y, greedy_times, fitted_values):
  #   plt.plot([x, x], [y, fitted_y], color='gray', linestyle='dotted')

  # Show the plot
  plt.savefig("plots/%s.sizescatter%s.png"%(fm, gate_set.value), dpi=300)
  plt.close()
  qr_squared = qr_value ** 2
  lr_squared = lr_value ** 2
  return (qr_squared, lr_squared)


def plot_bench_custom_label (bn):
  # if greedy:
  #   suffix = '.qasm.greedy.log'
  # else:
  #   suffix = '.qasm.search.log'
  (tq, szq, gtq, _) = log_from_bench(bn, Tool.quartz)
  (qtg, qsg, qts, qss) = split(tq, szq, gtq)

  fig, ax = plt.subplots()

  ax_inset = ax.inset_axes([0.5, 0.5, 0.5, 0.5])  # (x, y, width, height)

  # Plot the greedy phase on the inset axis



  if (len(tq) >= 2):
    ax.plot(qtg + qts, qsg + qss, marker = 's', label='Quartz', color = 'blue')
    ax_inset.plot(qtg, qsg,  marker = 's',label='Quartz', color='blue')
    # ax.set_xscale('log')
    # plt.xscale('log')
    # plt.
    # plt.plot(tq[-1], szq[-1], marker = 's', color = 'blue')
    # plt.axvline(x=gtq, color='red', linestyle='--')
  (tl, szl, gtl, gsl) = log_from_bench(bn, Tool.lopt)
  (ltg, lsg, _, lss) = split(tl, szl, gtl)
  if (len(tl) >= 2):
    plt.plot(tl, szl, marker = 'o', label='Local optimizer', color = 'green')
    plt.plot(tl[-1], szl[-1], marker = 'o', color = 'green')
    print(ltg, lsg, gtl, tl)
    ax_inset.plot(ltg + [gtl], lsg + [gsl], label='Local optimizer', color='green',  marker = 'o')
    # plt.axvline(x = gtl, color='red', linestyle='--')
  # ax_inset.set_title('Zoomed Greedy Phase')
  ax.set_xlabel('Time')
  ax.set_ylabel('Circuit Size')
  ax_inset.legend(loc='upper right')
  ax.indicate_inset_zoom(ax_inset)
  ax.set_title('%s: Circuit Size vs. Time'%(bn.capitalize()))
  fig.tight_layout()
  plt.savefig("plots/%s.inset.png"%(bn), dpi=300)


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
# bench_list = sorted(remove_bench(bench_list, 'gf'))
# bench_list = sorted(remove_bench(bench_list, 'qft'))

skip_list = []
skip_list = ["qcla_adder_10", "shor_7_mod_15_n18_from_python",  "gf2^128_mult", "gf2^131_mult", "gf2^163_mult", "gf2^64_mult"]
# qcla_adder_10 450 451
bench_list = list(filter (lambda x : not(x in skip_list), bench_list))

# curr_list = sorted(bench_list[0:4])


curr_list = bench_list
# print(curr_list)
# exit()


def cols_from_log (l):
  (tvs, sizes, gt, gv) = l
  return (sizes[0], sizes[-1], gt, gv)

# print(cols_from_log(log_from_bench ("adder_8", True)))
# print(cols_from_log(log_from_bench ("adder_8", False)))

def create_plots(curr_list, gate_set):
  quartz_logs = list (map(lambda x: (x, log_from_bench(x, Tool.quartz, gate_set)), curr_list))
  lopt_logs = list (map(lambda x: (x, log_from_bench(x, Tool.lopt, gate_set)), curr_list))
  for x in curr_list:
    print(x)
    plot_bench_inset(x, gate_set)
  return 0

def get_out_file (bn, tool, gate_set):
  return BASE_DIR + bn + "/" + bn + gate_set.value + (".%s.output" % (Tool.name(tool)))

def get_bench_file (bn, gate_set):
  if gate_set == GateSet.nam:
    if bn.startswith("qaoa_n"):
      return BASE_DIR + bn + "/" + bn + ".qasm"
    return BASE_DIR + bn + "/" + bn + ".qasm.preprocessed"
  else:
    return BASE_DIR + bn + "/" + bn + gate_set.value + ".qasm"

def create_plot_file(curr_list):
  header = "\\begin{figure*}[htbp]\n\t\centering\n"
  footer = "\caption{Size vs. Time plots}\n\end{figure*}\n"
  count = 0
  s=""
  for x in curr_list:
    if count % 3 == 0:
      s+=header
    s+="\t\includegraphics[width=0.3\linewidth]"
    s+="{plots/%s.inset.png}\quad\n"%(x)
    count+=1
    if count % 3 == 0:
      s+=footer
  if count % 3 != 0:
    s+=footer
  return s

def create_tables (curr_list, write, gate_set):
  quartz_logs = list (map(lambda x: (x, log_from_bench(x, Tool.quartz, gate_set)), curr_list))
  lopt_logs = list (map(lambda x: (x, log_from_bench(x, Tool.lopt, gate_set)), curr_list))
  headers = ["Name", "Original", "QUARTZ", "LOPT", "Original CX", "QUARTZ CX", "LOPT CX"]
  tab = []
  # LATEX IT
  for ((bn, q), (bnd, l)) in zip(quartz_logs, lopt_logs):
    print("bench = ", bn)
    (q0, qf, gtq, gqf) = cols_from_log(q)
    (l0, lf, gtl, glf) = cols_from_log(l)
    qfile = get_out_file (bn, Tool.quartz, gate_set)
    lfile = get_out_file (bn, Tool.lopt, gate_set)
    assert(bn == bnd)
    im = (1 - (float(lf)/float(qf))) * 100
    if gtl == 0.0:
      print(Colors.RED, bn, Colors.RESET)
    elif (q0 == l0):
      tab.append([bn, q0, qf, lf, qisk.get_cx_count_fast(get_bench_file(bn, gate_set)), qisk.get_cx_count_fast(qfile), qisk.get_cx_count_fast(lfile)])
    else:
      print(Colors.RED, bn, q0, l0, Colors.RESET)

  tab_len = len(tab)
  if (len(tab) > 30 and write):
    h = int(tab_len/2)
    tab1 = tab[0:h]
    tab2 = tab[h:]
    ltab1 = tabulate(tab1, headers=headers, tablefmt="latex")
    ltab2 = tabulate(tab2, headers=headers, tablefmt="latex")
    ext = gate_set.value + ".tex"
    with open("prelim1" + ext, "w") as f:
      f.write(ltab1)
    with open("prelim2" + ext, "w") as f:
      f.write(ltab2)
  else:
    print(tab)
    print("not the case yet")


def create_swap_count(c, gate_set):
  bn = "hwb6"
  optl = qisk.get_swap_count(get_out_file(bn, Tool.lopt, gate_set), nam_basis)
  optq = qisk.get_swap_count(get_out_file(bn, Tool.quartz, gate_set), nam_basis)
  orig = qisk.get_swap_count(get_bench_file(bn, gate_set), nam_basis)
  return (orig, optq, optl)

def filter_fm (p, c):
  return [s for s in c if s.startswith(p) and s.endswith("from_python")]

bench_list = sorted(listdir('benchmarks'))
bench_list.remove('make')
bench_list.remove('original')
bench_list.remove('vqe_n24')
bench_list.remove('shor_n5')
bench_list.remove('barenco_tof_10')
bench_list.remove('gf2^128_mult')
bench_list.remove('gf2^163_mult')
bench_list.remove('gf2^131_mult')
bench_list.remove('gf2^64_mult')

curr_list = []

# curr_list += ['hhl_n5_from_python', 'multiplier_n45', 'multiplier_n75', 'qaoa_n6', 'qaoa_n6_from_python', 'qaoa_n8_from_python', 'qft_n16_from_python', 'qft_n18', 'qft_n24_from_python', 'qft_n29', 'qft_n30_from_python', 'qft_n4_from_python', 'qft_n8_from_python', 'shor_7_mod_15_n12_from_python', 'shor_7_mod_15_n16_from_python', 'shor_7_mod_15_n18_from_python', 'shor_7_mod_15_n8_from_python']
# #
# curr_list += ['adder_8', 'barenco_tof_3', 'barenco_tof_4', 'barenco_tof_5', 'csla_mux_3', 'csum_mux_9', 'gf2^10_mult', 'gf2^16_mult', 'gf2^32_mult', 'gf2^4_mult', 'gf2^5_mult', 'gf2^6_mult', 'gf2^7_mult', 'gf2^8_mult', 'gf2^9_mult', 'grover_5', 'grover_n15_from_python', 'grover_n3_from_python', 'grover_n5_from_python', 'grover_n9_from_python', 'ham15-high', 'ham15-low', 'ham15-med', 'hhl_n10', 'hhl_n7_from_python', 'hwb6', 'rc_adder_6']
# curr_list += ['hhl_n7', 'hhl_n9_from_python', 'mod5_4', 'mod_adder_1024', 'mod_mult_55', 'mod_red_21', 'qaoa_n24_from_python', 'qcla_adder_10', 'qcla_com_7', 'qcla_mod_7', 'qft_n160', 'qft_n320', 'qft_n63', 'tof_10', 'tof_3', 'tof_4', 'tof_5', 'vbe_adder_3']

#  CODE FOR TABLES
#
# bench_list = remove_bench(bench_list, "barenco")
# bench_list = remove_bench(bench_list, "tof")
# bench_list = remove_bench(bench_list, "csla")
# bench_list = remove_bench(bench_list, "qcla")
# bench_list = remove_bench(bench_list, "gf")
# print(create_tables(bench_list, True, GateSet.nam))
# #


#  CODE FOR PLOTS

# curr_list += ['gf2^10_mult', 'gf2^16_mult', 'gf2^32_mult', 'gf2^4_mult', 'gf2^5_mult', 'gf2^6_mult', 'gf2^7_mult', 'gf2^8_mult', 'gf2^9_mult']
# curr_list += filter_fm (bench_list)
# curr_list.remove("shor_7_mod_15_n18_from_python")
# print(curr_list)
# create_tables(curr_list, True, GateSet.clifft)
# print(curr_list)
# print(create_plots(curr_list, GateSet.clifft))

# print(create_swap_count(curr_list))
# print(create_plot_file(curr_list))

# print(plot_scatter(curr_list, GateSet.clifft, "qaoa"))
# # curr_list = filter_fm("hhl", bench_list)
# # print(curr_list)
# print(plot_scatter(curr_list, GateSet.clifft, "hhl"))
# curr_list = filter_fm("qft", bench_list)
# print(plot_scatter(curr_list, GateSet.clifft, "qft"))
# # curr_list = filter_fm("grover", bench_list)
# curr_list = ['grover_n15_from_python', 'grover_n3_from_python', 'grover_n5_from_python', 'grover_n9_from_python']
# # print(curr_list)
# # print(plot_scatter(curr_list, GateSet.clifft, "grover"))
# curr_list = filter_fm("vqe", bench_list)
# print(plot_scatter(curr_list, GateSet.clifft, "vqe"))
# curr_list = filter_fm("shor", bench_list)
# print(plot_scatter(curr_list, GateSet.clifft, "shor"))

# curr_list = ['gf2^10_mult', 'gf2^16_mult', 'gf2^32_mult', 'gf2^4_mult', 'gf2^5_mult', 'gf2^6_mult', 'gf2^7_mult', 'gf2^8_mult', 'gf2^9_mult']
# print(plot_scatter(curr_list, GateSet.clifft, "gf"))


# curr_list = filter_fm("vqe", bench_list)
# print(plot_size(curr_list, GateSet.nam, "vqe"))

curr_list = ['qaoa_n10_p4' ,'qaoa_n12_p4' ,'qaoa_n14_p4' ,'qaoa_n16_p4' ,'qaoa_n20_p4' ,'qaoa_n22_p4' ,'qaoa_n8_p4' ,'qaoa_n24_p4' ,'qaoa_n26_p4' ,'qaoa_n28_p4' ,'qaoa_n30_p4' ,'qaoa_n4_p4' ,'qaoa_n18_p4' ,'qaoa_n6_p4']
print(plot_size(curr_list, GateSet.clifft, "qaoa"))