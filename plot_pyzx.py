import os
from tabulate import tabulate
from scipy.stats import gmean
# import matplotlib.pyplot as plt
path = "feyn_benchmarks/"
import csv
from enum import Enum
import re
import statistics


class Family (Enum):
  # grover = ["11", "13", "15"]
  grover = ["9", "11", "13","15"]
  hhl = ["7",  "9",] #11, 13
  qft = ["8",  "16", "24", "30"]
  # qftqis = ["8", "16", "32", "48", "64", "80", "96", "128"]
  qftqis = ["48", "64", "80", "96"]
  shor = ["10","12","14","16"]
  # vqe = ["16","20", "24", "28"]
  vqe = ["12", "16", "20","24"]
  bwt = ["17", "21"]
  boolsat = ["28", "30"]
  sqroot = ["42", "54"] # lopt??
  statevec = ["4", "5", "6"] # 5,  "7", "8"??
  hwb = ["8", "10", "11", "12"] # "12"
  # vqe = ["12", "16", "20", "24"]
  # gf = ["8","16", "32"]
  gf = ["32", "64", "128"]
  ham = ["med", "high"]


  arithmetic = ["adder\_8", "barenco\_tof\_10", "hwb6", "mod5\_4", "multiplier\_n45", "multiplier\_n75"]
  lin_grover = ["6", "7", "8", "9", "10", "11", "12", "14"]
  lin_hhl = ["5", "7",  "9"] #13
  lin_qft = ["4", "8",  "16", "24", "30"]
  lin_shor =  ["8", "9", "10", "11", "12", "13", "14"]
  lin_qftqis = ["16", "32", "64", "80", "96", "128"]

  # lin_shor = ["8", "10", "11", "12", "13"]
  lin_vqe = ["4", "6", "8", "12", "14", "16", "20"]
  lin_gf = []
  lin_arithmetic = ["adder_8", "barenco_tof_10", "hwb6", "mod5_4", "multiplier_n45", "multiplier_n75"]


  def name (fam):
    if fam == Family.grover or fam == Family.lin_grover:
      return "grover"
    if fam == Family.qftqis or fam == Family.lin_qftqis:
      return "qft"
    if fam == Family.hhl or fam == Family.lin_hhl:
      return "hhl"
    if fam == Family.qft or fam == Family.lin_qft:
      return "qft"
    if fam == Family.shor or fam == Family.lin_shor:
      return "shor"
    if fam == Family.vqe or fam == Family.lin_vqe:
      return "vqe"
    if fam == Family.arithmetic or fam == Family.lin_arithmetic:
      return "arith"
    if fam == Family.gf or fam == Family.lin_gf:
      return "gf"
    if fam == Family.ham:
      return "ham15"
    if fam == Family.vqe:
      return "vqe"
    if fam == Family.bwt:
      return "bwt"
    if fam == Family.boolsat:
      return "boolsat"
    if fam == Family.sqroot:
      return "sqroot"
    if fam == Family.statevec:
      return "statevec"
    if fam == Family.hwb:
      return "hwb"
    else:
      print("NOT IMPLEMENTED")

  def file (fam, var):
    if fam == Family.grover or fam == Family.lin_grover:
      return "grover_n%s_from_python_ts2"%(var)
    if fam == Family.hhl or fam == Family.lin_hhl:
      return "hhl_n%s_from_python"%(var)
    if fam == Family.qft or fam == Family.lin_qft:
      return "qft_n%s_from_python"%(var)
    if fam == Family.qftqis or fam == Family.lin_qftqis:
      return "qft_n%s_from_qiskit"%(var)
    if fam == Family.shor or fam == Family.lin_shor:
      return "shor_7_mod_15_n%s_from_python"%(var)
    if fam == Family.vqe or fam == Family.lin_vqe:
      return "vqe_n%s_from_python"%(var)
    if fam == Family.hwb:
      return "hwb%s"%(var)
    if fam == Family.arithmetic or fam == Family.lin_arithmetic:
      return var
    if fam == Family.gf or fam == Family.lin_gf:
      return "gf2^%s_mult"%(var)
    if fam == Family.ham:
      return "ham15-%s"%(var)
    if fam == Family.bwt:
      return "nwq_binary_welded_tree_n%s"%(var)
    if fam == Family.boolsat:
      return "nwq_boolean_satisfaction_n%s"%(var)
    if fam == Family.sqroot:
      return "nwq_square_root_n%s"%(var)
    if fam == Family.statevec:
      return "nwq_statevector_n%s"%(var)
    else:
      print("NOT IMPLEMENTED")

  def lsqubits (fam):
    ham_qubits = ["17", "20"]
    if fam == Family.ham:
      return ham_qubits
    elif fam == Family.gf:
      return [str(3*int(x)) for x in fam.value]
    else:
      return fam.value

  def labels (fam, var):
    if fam == Family.grover or fam == Family.lin_grover:
      return "grover\_n%s"%(var)
    if fam == Family.hhl or fam == Family.lin_hhl:
      return "hhl\_n%s"%(var)
    if fam == Family.qft or fam == Family.lin_qft:
      return "qft\_n%s"%(var)
    if fam == Family.qftqis or fam == Family.lin_qftqis:
      return "qft\_n%s"%(var)
    if fam == Family.shor or fam == Family.lin_shor:
      return "qpe\_n%s"%(var)
    if fam == Family.vqe or fam == Family.lin_vqe:
      return "vqe\_n%s"%(var)
    if fam == Family.arithmetic or fam == Family.lin_arithmetic:
      return var
    if fam == Family.ham:
      return "ham15-%s"%(var)
    if fam == Family.hwb:
      return "hwb%s"%(var)
    if fam == Family.gf or fam == Family.lin_gf:
      return "gf2\^{}%s\_mult"%(var)
    else:
      print("NOT IMPLEMENTED")

  def ls(fam):
    return list(map(lambda x : Family.file(fam, x), fam.value))

  def lslabels(fam):
    return list(map(lambda x : Family.labels(fam, x), fam.value))

def read_file(f):
  with open (f) as file:
    return file.read()

def read_log (f):
  try:
    d = read_file(f)
  except Exception as D:
    print(f'File {f} not exist')
    return (None, None, None)
  if d == "" or d.strip() == "TIMEOUT":
    print('Empty or timeout')
    return (None, None, None)
  d = d.replace('~', '-').replace('Ee', 'E')
  rows = d.split("\n")
  data_points = rows[0].split(";")
  print(f)
  print(data_points)
  assert (len(data_points) >= 2)
  if data_points[1].strip() == '':
    data_points[1] = rows[1].split(";")[0].strip()
    print(data_points)
  def parse_point (p):
    p = p.strip()
    values = p.strip("()").split(",")
    values = [value.strip() for value in values]
    print(values)
    return (float(values[0]), int(float(values[1])))

  p1 = parse_point(data_points[0])
  p2 = parse_point(data_points[1])
  # initial size, final size, time
  return (p1[1], p2[1], p2[0])

def parse_plot_times (bn, sizes):
  def log_file (size):
    return "feyn_benchmarks/%s.lopt.size.%d.log"%(bn, size)
  tups = list(map(lambda x: read_log(log_file(x)), sizes))
  quality = [x[1] for x in tups]
  times = [x[2] for x in tups]
  return (quality, times)

def create_family_rows (fam):
  curr_list = Family.ls (fam)
  qlist = Family.lsqubits(fam)
  flogs =  [read_log(path + f + ".pyzx.log") for f in curr_list]
  llogs = [read_log(path + f + ".lopt.100.400.converge.log") for f in curr_list]
  print(fam, curr_list)
  fam_name = "\multirow{%d}{*}{%s}"%(len(curr_list), Family.name(fam))
  tab = []
  ratsizes = [[], []]
  rattimes = []
  print(fam_name)
  def format_pct (p):
    if p == 0.0:
      return "{:.1f}\%".format(p)
    return "-{:.1f}\%".format(p)
  for (bn, f, l) in zip (qlist, flogs, llogs):
    (ti, tf, t) = f
    print(bn)
    (ti2, tf2, t2) = l
    if ti == None:
      ti = ti2
      assert (ti is not None)
      t = 12*60*60
      tratio = None
      print(bn, t, t2)
      time_ratio = abs(round(float(t)/t2, 2))
      rattimes.append(time_ratio)
      pct_lopt = 100 * (ti - tf2)/float(ti)
      ratsizes[1].append(pct_lopt)
      tab.append([fam_name, bn, ti, "T.O.", "\\textbf{%s}"%round(t2, 1), time_ratio, "T.O.", format_pct(pct_lopt)])
    else:
      tratio = abs(round(float(tf)/tf2, 2))
      time_ratio = abs(round(float(t)/t2, 2))
      rattimes.append(time_ratio)
      pct_lopt = 100 * (ti - tf2)/float(ti)
      pct_feyn = 100 * (ti - tf)/float(ti)
      ratsizes[0].append(pct_feyn)
      ratsizes[1].append(pct_lopt)
      if t2 > t:
        tab.append([fam_name, bn, ti, "\\textbf{%s}"%round(t, 1), round(t2, 1), time_ratio, format_pct(pct_feyn), format_pct(pct_lopt)])
      else:
        tab.append([fam_name, bn, ti, round(t, 1), "\\textbf{%s}"%round(t2, 1), time_ratio, format_pct(pct_feyn), format_pct(pct_lopt)])
    print(bn, ratsizes[-1], rattimes[-1])
    fam_name = ""
  return (tab, ratsizes, rattimes)


def create_tables (fam_list):

  def flatten(l):
    size_feyn = []
    size_lopt = []
    tab = []
    rattimes = []
    for sublist in l:
      tab+=sublist[0]
      size_feyn+=sublist[1][0]
      size_lopt+=sublist[1][1]
      rattimes += sublist[2]
    return (tab, size_feyn, size_lopt, rattimes)
  print("loop begine")
  (tab, size_feyn, size_lopt, rattimes) = flatten(list(map (lambda x: create_family_rows(x), fam_list)))
  # for (bn, f, l) in zip (names, feyn_logs, lopt_logs):
  #   (ti, tf, t) = f
  #   (ti2, tf2, t2) = l
  #   if ti == None:
  #     ti = ti2
  #     t = 36000
  #     tratio = None
  #     print(bn, t, t2)
  #     time_ratio = abs(round(float(t)/t2, 2))
  #     # ratsizes.append(tratio)
  #     rattimes.append(time_ratio)
  #     tab.append([bn, ti, "T.O.", tf2, "N/A", t, t2, time_ratio])
  #   else:
  #     tratio = abs(round(float(tf)/tf2, 2))
  #     time_ratio = abs(round(float(t)/t2, 2))
  #     ratsizes.append(tratio)
  #     rattimes.append(time_ratio)
  #     tab.append([bn, ti, tf, tf2, tratio, t, t2, time_ratio])
  custom_header = (
      " & &  & \\multicolumn{2}{c}{Time} & &  \\multicolumn{2}{c}{T Count Reduction}  \\\\ \\cmidrule(lr){4-5} \\cmidrule(lr){7-8}\n"
      "  Family & Qubits & Input T Count & \\feyntool{} & \\algname{} & \\algname{} speedup & \\feyntool{} & \\algname{} \\\\ \n"
  )
  def format_pct (p):
    return "-{:.1f}\%".format(p)
  def find_line_position(input_string):
    first_newline_index = input_string.find('\n')
    return first_newline_index
  def mean(x):
    x = [abs(y)for y in x]
    return str(round(gmean(x), 2))
  ltab = tabulate(tab, tablefmt="latex_raw")
  pos = find_line_position(ltab)
  ltab = "\\begin{tabular}{cccccccccc}" + "\n" + custom_header + ltab[pos:]
  ltab = ltab.replace(r"\multirow", r"\midrule\multirow")

  avg_feyn = statistics.mean([x for x in size_feyn if x!="T.O."])
  avg_lopt = statistics.mean([x for x in size_lopt if x!="T.O."])
  # print("percentages", [x for x in ratsizes[0] if x!="T.O."])
  ltab = ltab.replace('\hline', '')
  mean_row = "\\midrule\n\\textbf{average} & & & & & %s & %s & %s \n"%(mean(rattimes), format_pct(avg_feyn), format_pct(avg_lopt))
  last = "\end{tabular}"
  ltab = ltab.replace(last, mean_row + last)
  with open("prelim.pyzx.tex", "w") as f:
    f.write(ltab)
  print("filename", "prelim.pyzx.tex")


def plot_times (families):
  def points_fam(fam):
    l = fam.value
    curr_list = list(map  (lambda x: Family.file(fam, x), l))
    logs = list(map (lambda x: read_log(path + x + ".lopt.60.log"), curr_list))
    print(logs)
    # nqs = list(map (lambda x: num_qubits(x), curr_list))
    # x = []
    # for (nq, l) in zip(nqs, logs):
      # x.append(float(l[1][0]))
      # x.append(float(l[1][0] * nq)/(x1 * nqs[0]))
    x = list(map (lambda x: float(x[0]), logs))
    yl = list(map (lambda x: x[2], logs))
    so = (list(zip(x, yl)))
    so2 = sorted(so, key=lambda x : x[0])
    (x, yl) = zip(*so2)
    return (fam, x, yl)
  points = [points_fam (x) for x in families]
  print(points)
  colors = ['#1f77b4', '#2ca02c', '#d62728', '#9467bd', '#ff7f0e']


  for (color, (fam, x, y)) in zip(colors, points):
    # for (xp, yp) in zip(x, y):
    # plt.scatter ([float(xp)/(160.0) for xp in x], y, label = Family.name(fam))
    plt.plot ([float(xp/1000) for xp in x], y, label = Family.name(fam), marker='o', color = color)
    f = "plots/linearity%s%s.png"%(Family.name(fam), "pyzx")
    print(x, y)
    # plt.ylabel('Optimization Time (s)', fontsize=20)
    # plt.xlabel("Circuit Size (in thousands)", fontsize=20)
    font = {'size'   : 16}

    plt.rc('font', **font)
    plt.xticks(fontsize=22)
    plt.yticks(fontsize=22)
    # plt.title("Linearity of the COAM algorithm with oracle timeout (%s)"%(Tool.extension()))
    plt.legend()
    # plt.subplots_adjust(bottom=0.2)
    # plt.subplots_adjust(left=0.2)
    plt.savefig(f)
    print(f)
    plt.close()

def read_voqc_log (f):
  d = read_file (f)
  rows = d.split("\n")
  # data_points = rows[0].split(";")
  # # print(data_points)
  # data_points.pop()
  # time_values = []
  # circuit_size_values = []
  # def parse_point (p):
  #   values = p.strip("()").split(",")
  #   return (float(values[0]), int(float(values[1])))
  # for point in data_points:
  #   (szv, tv) = parse_point(point)
  #   time_values.append(tv)
  #   circuit_size_values.append(szv)
  gt = None
  # gt = parse_point(rows[1].split(";")[0])[0]
  gv = None
  gate_before=None
  pattern_before = r"Input circuit has (\d+) T gates"
  pattern_v = r", T : (\d+)"
  # pattern_t = r"Voqc total takes (\d+(\.\d+)?) seconds"
  pattern_t = r"Optimization took (\d+(\.\d+)?) seconds"

  for row in rows:
    # print(row)
    match = re.search(pattern_t, row)
    if match:
      gt = match.group(1)
    match = re.search(pattern_v, row)
    if match:
      gv = int(match.group(1))
    match = re.search(pattern_before, row)
    if match:
      gate_before = int(match.group(1))

  # gv = parse_point(rows[1].split(";")[0])[1]
  # print("queso file", f)
  # print("time = ", time_values[-1])
  # print("size = ", circuit_size_values[-1])
  return (gate_before, gv,gt)


class Tool (Enum):
  voqc = 1
  pyzx = 2
  lopt_pyzx = 3
  lopt_voqc = 4

BASE_DIR = "feyn_benchmarks"

def log_from_bench(bn, tool):
  print("bench = ", bn)

  path = BASE_DIR + "/" + bn
  if tool == Tool.voqc:
    return read_voqc_log (path +  ".voqc.combined.log")
  elif tool == Tool.pyzx:
    return read_log(path + ".pyzx.log")
  elif tool == Tool.lopt_pyzx:
    return read_log (path + ".lopt.pyzx.100.400.converge.log")
  elif tool == Tool.lopt_voqc:
    return read_log (path + ".lopt.voqc.log")
  else:
    raise FileExistsError


def generate_table_pengyu (configs, name_list):
  my_csv=[["-" for _ in range(17)] for _ in range(50)]


  for i,name in enumerate(name_list):
    my_csv[i][0]=name
  for i,name in enumerate(name_list):

    log=log_from_bench(name, configs[1])
    my_csv[i][1]=log[0]


    for j,config in enumerate(configs):
      offset=2*j+2
      if j>3:
        offset+=1
      log=log_from_bench(name, config)
        # print(name, log)
      if log[1] is not None:
        my_csv[i][offset+1]=log[1]
      if log[2] is not None:
        my_csv[i][offset]=log[2]
  with open('pengyu_result_feynman.csv', 'w', newline='') as file:
    writer = csv.writer(file)
    writer.writerows(my_csv)



def generate_table_mingkuan (configs, name_list):
  my_csv=[["-" for _ in range(17)] for _ in range(len(name_list))]


  for i,name in enumerate(name_list):
    my_csv[i][0]=name
  for i,name in enumerate(name_list):

    log=log_from_bench(name, configs[0])
    my_csv[i][1]=log[0]


    for j,config in enumerate(configs):
      offset=2*j+2
      if j>3:
        offset+=1
      log=log_from_bench(name, config)
      # print(name, log)
      if log[1] is not None:
        my_csv[i][offset+1]=log[1]
      if log[2] is not None:
        my_csv[i][offset]=log[2]
  with open('mingkuan_result_pyzx.csv', 'w', newline='') as file:
    writer = csv.writer(file)
    writer.writerows(my_csv)



files = [f for f in os.listdir(path) if os.path.isfile(os.path.join(path, f))]
nwq_names=["nwq_binary_welded_tree_n17", "nwq_binary_welded_tree_n21", "nwq_boolean_satisfaction_n28", "nwq_boolean_satisfaction_n30", "nwq_boolean_satisfaction_n32", "nwq_boolean_satisfaction_n34",  "nwq_square_root_n42", "nwq_square_root_n48", "nwq_square_root_n54", "nwq_square_root_n60",  "nwq_statevector_n5",  "nwq_statevector_n6", "nwq_statevector_n7", "nwq_statevector_n8"]

bench_files = [f for f in files if (not('output' in f))]
bench_list = list(set([f.split('.')[0] for f in bench_files]))
bench_list.remove('hhl_n9_from_python')
bench_list.remove('shor_7_mod_15_n16_from_python')
bench_list.remove('shor_7_mod_15_n18_from_python')
bench_list.sort()
# print((bench_list))
curr_list = []
name_list = []
fam_list =  [Family.bwt, Family.hhl, Family.hwb, Family.qftqis, Family.shor, Family.sqroot]
# fam_list =  [Family.hwb, Family.gf, Family.grover, Family.hhl, Family.qftqis, Family.shor]
for fam in fam_list:
  name_list.extend(fam.ls())

name_list+=nwq_names
# create_tables(fam_list)

# print(parse_plot_times("hhl_n7_from_python", [2, 5, 15, 30, 60, 120, 240, 480, 960, 1920, 3840, 7680]))

# print(name_list)
# configs = [Tool.voqc, Tool.feynman,  Tool.lopt_voqc, Tool.lopt_feynman]
configs = [ Tool.pyzx,  Tool.lopt_pyzx]

generate_table_mingkuan(configs, ['gf2^128_mult', 'gf2^32_mult', 'gf2^64_mult', 'grover_n11_from_python',
                                  'grover_n11_from_python_ts', 'grover_n11_from_python_ts2', 'grover_n13_from_python',
                                  'grover_n13_from_python_ts2', 'grover_n15_from_python', 'grover_n15_from_python_ts',
                                  'grover_n15_from_python_ts2', 'grover_n9', 'grover_n9_from_python',
                                  'grover_n9_from_python_ts2', 'hhl_n7_from_python', 'hhl_n9_from_python', 'hwb10',
                                  'hwb11', 'hwb12', 'hwb6', 'hwb8', 'nwq_binary_welded_tree_n17',
                                  'nwq_binary_welded_tree_n21', 'nwq_boolean_satisfaction_n24',
                                  'nwq_boolean_satisfaction_n28', 'nwq_boolean_satisfaction_n30',
                                  'nwq_boolean_satisfaction_n32', 'nwq_boolean_satisfaction_n34', 'nwq_multiplier_n100',
                                  'nwq_multiplier_n200', 'nwq_multiplier_n300', 'nwq_multiplier_n400',
                                  'nwq_square_root_n21', 'nwq_square_root_n30', 'nwq_square_root_n42',
                                  'nwq_square_root_n48', 'nwq_square_root_n54', 'nwq_square_root_n60',
                                  'nwq_statevector_n4', 'nwq_statevector_n5', 'nwq_statevector_n6',
                                  'qft_n16_from_qiskit', 'qft_n32_from_qiskit', 'qft_n48_from_qiskit',
                                  'qft_n64_from_qiskit', 'qft_n80_from_qiskit', 'qft_n96_from_qiskit',
                                  'shor_7_mod_15_n10_from_python', 'shor_7_mod_15_n12_from_python',
                                  'shor_7_mod_15_n14_from_python', 'shor_7_mod_15_n16_from_python',
                                  'shor_7_mod_15_n18_from_python', 'shor_7_mod_15_n8_from_python'])
# generate_table_pengyu (configs, name_list)

# for fam in [Family.hwb, Family.gf, Family.grover, Family.hhl, Family.qftqis, Family.shor]:
#   curr_list += Family.ls (fam)
#   name_list += Family.lslabels(fam)
# fam_list =  [Family.hwb, Family.gf, Family.grover, Family.hhl, Family.qftqis, Family.shor]
# # fam_list =  [Family.hwb]
# create_tables(fam_list)
# plot_times([Family.hwb, Family.shor, Family.qftqis])
# plot_times([Family.lin_qftqis])
