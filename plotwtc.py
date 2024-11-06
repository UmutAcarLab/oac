import matplotlib.pyplot as plt
import glob
import os
from tabulate import tabulate
from os import listdir
from enum import Enum
# import qiskit_api as qisk
from scipy.stats import linregress
from scipy.interpolate import interp1d
import numpy as np
from numpy.polynomial.polynomial import polyfit
from family import *
from scipy.stats import gmean
import statistics
from matplotlib.ticker import MultipleLocator

class GateSet (Enum):
  nam = ""
  clifft = ".clifft"
  ibm = ".ibm"

class Tool (Enum):
  quartz = 1
  lopt = 2
  queso = 3
  voqc = 4
  lopt_quartz = 5
  lopt_queso = 6
  lopt_voqc = 7
  pyzx = 8
  lopt_pyzx = 9

  def extension (self):
    if self == Tool.lopt_quartz:
      return ".lopt."+ "greedy" + "."
    elif self == Tool.lopt_queso:
      return ".lopt."+ "queso" + "."
    elif self == Tool.lopt_voqc:
      # return ".lopt."+"voqc"+"."
      return ".lopt.convergence.voqc."
    elif self == Tool.lopt_pyzx:
      return ".lopt.convergence.pyzx."
    else:
      return ""
  def config() :
    return ""
    # return ".005.lopt.queso."

  def name (t):
    if t == Tool.quartz:
      return "quartz"
    elif t == Tool.queso:
      return "queso"
    elif t == Tool.voqc:
      return "voqc"
    elif t == Tool.pyzx:
      return "pyzx"
    else:
      return "lopt.0.01"


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

# def read_queso_log (f):
#   d = read_file (f)
#   print(f)
#   try:
#     rows = d.split("\n")
#     data_points = rows[0].split(";")
#     data_points.pop()
#     time_values = []
#     circuit_size_values = []
#     def parse_point (p):
#       values = p.strip("()").split(",")
#       return (float(values[0]), int(float(values[1])))
#     for point in data_points:
#       (szv, tv) = parse_point(point)
#       time_values.append(tv)
#       circuit_size_values.append(szv)
#     gt = time_values[-1]
#     # gt = parse_point(rows[1].split(";")[0])[0]
#     gv = circuit_size_values[-1]
#     # gv = parse_point(rows[1].split(";")[0])[1]
#     # print("queso file", f)
#     # print("time = ", time_values[-1])
#     # print("size = ", circuit_size_values[-1])
#     return (time_values, circuit_size_values, gt, gv)
#   except:
#     return (["Unknown Error"], ["Unknown Error"], "Unknown Error", "Unknown Error")

def read_queso_log (f):
  d = read_file (f)
  print(f)
  try:
    lines = d.split('\n')
    lines.reverse()
    for line in lines:
      if "(" in line:
        datapoints= line.split(";")
        datapoints.reverse()
        for datapoint in datapoints:
          if ")" in datapoint:
            datapoint= datapoint.strip("()")
            gt= float(datapoint.split(",")[0])
            gv= int(float(datapoint.split(",")[1]))
            return ([0],[0],gv,gt)
    return (["Unknown Error"], ["Unknown Error"], "Unknown Error", "Unknown Error")
  except:
    return (["Unknown Error"], ["Unknown Error"], "Unknown Error", "Unknown Error")

def read_pyzx_log (f):
  d = read_file (f)
  rows = d.split("\n")
  try:
    total_time = rows[1].split(' ')[-1]
    tcount = rows[0].split(' ')[-1]
    return ([0],[rows[0].split(' ')[-2]],total_time, tcount)
  except:
    return (["Unknown Error"], ["Unknown Error"], "Unknown Error", "Unknown Error")

def read_voqc_log (f):
  d = read_file (f)
  rows = d.split("\n")
  data_points = rows[0].split(";")
  # print(data_points)
  data_points.pop()
  time_values = []
  circuit_size_values = []
  def parse_point (p):
    values = p.strip("()").split(",")
    return (float(values[0]), int(float(values[1])))
  for point in data_points:
    (szv, tv) = parse_point(point)
    time_values.append(tv)
    circuit_size_values.append(szv)
  gt = None
  # gt = parse_point(rows[1].split(";")[0])[0]
  gv = None
  pattern_v = r"After optimization, the circuit uses (\d+) gates"
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

  # gv = parse_point(rows[1].split(";")[0])[1]
  # print("queso file", f)
  # print("time = ", time_values[-1])
  # print("size = ", circuit_size_values[-1])
  return (time_values, circuit_size_values, gt, gv)

# def read_quartz_log (f):
#   d = read_file (f)
#   lines = d.split('\n')
#   greedy_points = lines[2].split(";")
#   # print(greedy_points)
#   search_points = lines[-2].split(";")
#   greedy_points.pop()
#   search_points.pop()
#   time_values = []
#   circuit_size_values = []
#   for point in greedy_points:
#     values = point.strip("()").split(",")
#     tv = float(values[0])
#     time_values.append(tv)
#     circuit_size_values.append(int(float(values[1])))
#   # if (len(time_values) <= 0):
#     # print("no times in ", f)
#     # return ([0], [0], 0)
#   (gt, gv) = (time_values[-1], circuit_size_values[-1])
#   for point in search_points:
#     values = point.strip("()").split(",")
#     tv = float(values[0])
#     time_values.append(tv + gt)
#     circuit_size_values.append(int(float(values[1])))
#   return (time_values, circuit_size_values, gt, gv)


def read_quartz_log (f):
  d = read_file (f)
  print(f)
  lines = d.split('\n')
  lines.reverse()
  for line in lines:
    if "(" in line:
      datapoints= line.split(";")
      datapoints.reverse()
      for datapoint in datapoints:
        if ")" in datapoint:
          datapoint= datapoint.strip("()")
          gt= float(datapoint.split(",")[0])
          gv= int(float(datapoint.split(",")[1]))
          return ([0],[0],gt,gv)
  return (["Unknown Error"], ["Unknown Error"], "Unknown Error", "Unknown Error")

def read_local_log (f):
  print('local', f)
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
    p = p.strip()
    values = p.strip("()").split(",")
    return (float(values[0].strip()), int(float(values[1].strip())))
  for point in data_points:
    (tv, szv) = parse_point(point)
    time_values.append(tv)
    circuit_size_values.append(szv)
  gt = parse_point(rows[1].split(";")[0])[0]
  gv = parse_point(rows[1].split(";")[0])[1]
  # print("local file", f)
  # print(gt, gv)
  return (time_values, circuit_size_values, gt, gv)

# def read_queso_log (f):
#   import re
#   log_line = read_file(f)
#   # Extract the numbers using regular expressions
#   seconds_match = re.search(r'optimized in ([\d.]+) seconds', log_line)
#   gate_count_match = re.search(r'Final gate count: (\d+)', log_line)

#   if seconds_match and gate_count_match:
#       seconds = int(seconds_match.group(1))
#       gate_count = int(gate_count_match.group(1))
#       return (gate_count, seconds)
#   else:
#       print("Pattern not found in the log line.", f)


def log_from_bench(bn, tool, gate_set):
  print("bench = ", bn)

  path = BASE_DIR + bn + "/" + bn + gate_set.value

  if tool == Tool.quartz:
    try :
      open(path + ".quartz.12.combined.log")
    except:
      return (["File Not Exist"], ["File Not Exist"], "File Not Exist", "File Not Exist")
    return read_quartz_log (path +  ".quartz.12.combined.log")
  elif tool == Tool.queso:
    try :
      open(path + ".queso.12.combined.log")
    except:
      return (["File Not Exist"], ["File Not Exist"], "File Not Exist", "File Not Exist")
    return read_queso_log(path + ".queso.12.combined.log")
  elif tool == Tool.voqc:
    try :
      open(path + ".voqc.combined.log")
    except:
      return (["File Not Exist"], ["File Not Exist"], "File Not Exist", "File Not Exist")
    return read_voqc_log (path + ".voqc.combined.log")
  elif tool == Tool.pyzx:
    try :
      open(path + ".pyzx.combined.log")
    except:
      return (["File Not Exist"], ["File Not Exist"], "File Not Exist", "File Not Exist")
    return read_pyzx_log (path + ".pyzx.combined.log")
  else:
    try:
      open(path +  "%s%slog"%(Tool.extension(tool), Tool.config()))
    except:
      return (["File Not Exist"], ["File Not Exist"], "File Not Exist", "File Not Exist")
    return read_local_log(path +  "%s%slog"%(Tool.extension(tool), Tool.config()))


def split (times, sizes, time):
  logs = list(zip(times, sizes))
  # print(times, sizes, time)
  # print(list(logs))
  before = [t for t in logs if t[0] <= time]
  after = [t for t in logs if t[0] >= time]
  # if after:
    # before.append(after[0])
  ub = list(zip(*before))
  ua = list(zip(*after))
  if ua == []:
    ua = [[], []]
  if ub == []:
    ub = [[], []]
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

import re
def num_qubits(bn):
  pattern = r'n(\d+)'

  match = re.findall(pattern, bn)
  if match:
    return int(match[0])
  else:
    return None

def plot_times (families, gate_set):
  def points_fam(fam):
    l = fam.value
    curr_list = list(map  (lambda x: Family.file(fam, x), l))
    logs = list(map (lambda x: log_from_bench(x, Tool.lopt, gate_set), curr_list))

    nqs = list(map (lambda x: num_qubits(x), curr_list))
    # x = []
    # for (nq, l) in zip(nqs, logs):
      # x.append(float(l[1][0]))
      # x.append(float(l[1][0] * nq)/(x1 * nqs[0]))
    x = list(map (lambda x: float(2 * x[1][0] - x[1][-1]), logs))
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
    f = "plots/linearity%s%s%s.png"%(Family.name(fam), Tool.extension(), gate_set.value)
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

  # def log_function(k, x):
  #     return k * x
  # x = np.linspace(1000, 120000, 100)  # Adjust the range as per your requirement

  # # Calculate y values using k = 2
  # k = 0.0035
  # y = log_function(k, x)
  # # plt.xlim(left=100, right=50000)
  # # plt.plot(x, y, label='y = m * x')
  # # plt.xscale('log')
  # # plt.title("Time vs. Size with oracle timeout (%s)"%(Tool.extension()))
  # # plt.ylabel("Time")
  # plt.xlabel("Circuit size")

  # plt.ylabel('Optimization Time (s)')
  # # plt.xlabel("Relative Circuit Size (ratio to smallest)")
  # plt.title("Linearity of the COAM algorithm with oracle timeout (%s)"%(Tool.extension()))
  # plt.legend()
  # # plt.xlim(left = 0, right=1000)
  # # plt.ylim(bottom =0, top=50)
  # # plt.xpltis.set_major_locator(MultipleLocator(1000))
  # plt.legend()
  # f = "plots/linearityscat%s%s.png"%(Tool.extension(), gate_set.value)
  # plt.savefig(f, dpi=300)
  # print(f)
  # plt.close()

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

def create_queso_tables(curr_list, names, write):
  queso_pre = "queso_runs/"
  if Tool.extension() == "0.01":
    queso_pre+='t-low/'
  elif Tool.extension() == "0.1":
    queso_pre+='t-mid/'
  queso_logs =  list (map(lambda x: read_queso_log(queso_pre + x + ".out"), curr_list))
  lopt_logs = list (map(lambda x: log_from_bench(x, Tool.lopt, GateSet.nam), curr_list))
  tab = []
  # LATEX IT
  qpcts = []
  lpcts = []
  ratsizes = []
  ratios = []
  for (bn, q, l) in zip(names, queso_logs, lopt_logs):
    (qf, tf) = q
    (l0, lf, gtl, glf) = cols_from_log(l)
    q0 = l0
    qpct = 0
    lpct = 0
    if q0 != lf:
      lpct = (round(float(lf-q0)/q0, 2) * 100)
    if q0 != qf:
      qpct = (round(float(qf-q0)/q0, 2) * 100)
    qrate = abs(round(float(qf - q0)/tf, 2))
    lrate = abs(round(float(lf - q0)/gtl, 2))
    if qrate == 0:
      rat = 1.0
    else:
      rat = round((lrate)/ (qrate), 2)
    pctdiff = round((float(lf)/float(qf)) - 1.0, 2) * 100
    qpcts.append(qpct)
    lpcts.append(lpct)
    ratsize = round(float(qf)/float(lf), 2)
    ratios.append(rat)
    ratsizes.append(ratsize)
    if lf < qf:
      lf = "\\textbf{" + str(lf) + "}"
    elif lf > qf:
      qf = "\\textbf{" + str(qf) + "}"
    tab.append([bn, q0, str(qf) + " (%d\%%)" %(qpct), str(lf) + " (%d\%%)" %(lpct), str(ratsize) + "x", qrate, lrate, str(rat) + "x", int(gtl), tf])

  custom_header = (
        " &  & \\multicolumn{3}{c}{Output Size and Improvement} &  \\multicolumn{3}{c}{Optimization Rate (gates/sec)} & &  \\\\ \\cmidrule(lr){3-5} \\cmidrule(lr){6-8}\n"
        "  Benchmark & Input Size & QS & S & QS/S & QS & S & S/QS  & Time (s) & QS Time (s) \\\\ \n"
    )

  def find_line_position(input_string):
      first_newline_index = input_string.find('\n')
      return first_newline_index
  def mean(x):
    x = [abs(y)for y in x]
    return str(round(gmean(x), 2))
  def meanp(x):
    x = [1 + abs(y)for y in x]
    return str(round(gmean(x), 2))
  ltab = tabulate(tab, tablefmt="latex_raw")
  pos = find_line_position(ltab)
  # insert header manually
  ltab = "\\begin{tabular}{rccccccccc}" + "\n" + custom_header + ltab[pos:]
  ltab = ltab.replace('hline', 'midrule')
  mean_row = "\\textbf{geomean} & & & & %s & &  & %s & & \n"%(mean(ratsizes), meanp(ratios))
  last = "\end{tabular}"
  ltab = ltab.replace(last, mean_row + last)
  ext = "%s.%s.tex"%("queso", Tool.extension())
  with open("prelim" + ext, "w") as f:
    f.write(ltab)
  print("filename", "prelim" + ext)

def create_tables (curr_list, names, write, gate_set):
  quartz_logs = list (map(lambda x: log_from_bench(x, Tool.queso, gate_set), curr_list))
  lopt_logs = list (map(lambda x: log_from_bench(x, Tool.lopt, gate_set), curr_list))
  t = "Q^*"
  headers = ["Name", "Input Size", t, "S", t+"/S", t, "S", "S/"+t,  "Time (s)"]
  tab = []
  # LATEX IT
  qpcts = []
  lpcts = []
  ratsizes = []
  ratios = []
  for (bn, q, l) in zip(names, quartz_logs, lopt_logs):
    (l0, lf, gtl, glf) = cols_from_log(l)
    (qtimes, qsizes, qtimes2, _) = split(q[0], q[1], gtl)
    if (bn.startswith('hhl_n5')) and len(qsizes) >= 2:
      qsizes = qsizes[1:]
    elif (bn.startswith('hhl_n13')):
      qsizes = [l0]
    (q0, qf) = (int(qsizes[0]), int(qsizes[-1]))
    print("qfinal = ", qf)
    if q0 < 1000:
      continue
    elif (q0 == l0 or l0 == 0):
      if len(qtimes2) == 0 and not (gtl >= 10000 and qtimes[-1] >= 10000):
        print("quartz hasn't run for this time", gtl, qtimes[-1])
        print(Colors.RED, bn, Colors.RESET)
      qpct = 0
      lpct = 0
      if q0 != lf:
        lpct = (round(float(lf-q0)/q0, 2) * 100)
      if q0 != qf:
        qpct = (round(float(qf-q0)/q0, 2) * 100)
      qrate = 1 + abs(round(float(qf - q0)/gtl, 2))
      lrate = 1 + abs(round(float(lf - q0)/gtl, 2))
      if qrate == 0:
        rat = 1.0
      else:
        rat = round((lrate)/ (qrate), 2)
      pctdiff = round((float(lf)/float(qf)) - 1.0, 2) * 100
      qpcts.append(qpct)
      lpcts.append(lpct)
      ratsize = round(float(qf)/float(lf), 2)
      ratios.append(rat)
      ratsizes.append(ratsize)
      if lf < qf:
        lf = "\\textbf{" + str(lf) + "}"
      tab.append([bn, q0, str(qf) + " (%d\%%)" %(qpct), str(lf) + " (%d\%%)" %(lpct), str(ratsize) + "x", qrate, lrate, str(rat) + "x", int(gtl)])
    else:
      print(Colors.RED, bn, q0, l0, Colors.RESET)

  tab_len = len(tab)
  if (len(tab) > 30 and write and False):
    h = int(tab_len/2)
    tab1 = tab[0:h]
    tab2 = tab[h:]
    ltab1 = tabulate(tab1, headers=headers, tablefmt="latex")
    ltab2 = tabulate(tab2, headers=headers, tablefmt="latex")
    ext = gate_set.value + ".%s.tex"%(Tool.extension())
    with open("prelim1" + ext, "w") as f:
      f.write(ltab1)
    with open("prelim2" + ext, "w") as f:
      f.write(ltab2)
    print("filenames", "prelim1" + ext, "prelim2" + ext)
  else:
    custom_header = (
        " &  & \\multicolumn{3}{c}{Output Size and Improvement} &  \\multicolumn{3}{c}{Optimization Rate (gates/sec)} &  \\\\ \\cmidrule(lr){3-5} \\cmidrule(lr){6-8}\n"
        "  Benchmark & Input Size & Q & S & Q/S & Q & S & S/Q  & Time (s) \\\\ \n"
    )

    def find_line_position(input_string):
      first_newline_index = input_string.find('\n')
      return first_newline_index
    def mean(x):
      x = [abs(y)for y in x]
      return str(round(gmean(x), 2))
    ltab = tabulate(tab, tablefmt="latex_raw")
    pos = find_line_position(ltab)
    # insert header manually
    ltab = "\\begin{tabular}{rcccccccc}" + "\n" + custom_header + ltab[pos:]
    ltab = ltab.replace('hline', 'midrule')
    mean_row = "\\textbf{geomean} & & & & %s & &  & %s & \n"%(mean(ratsizes), mean(ratios))
    last = "\end{tabular}"
    ltab = ltab.replace(last, mean_row + last)
    ext = gate_set.value + ".%s.tex"%(Tool.extension())
    with open("prelim" + ext, "w") as f:
      f.write(ltab)
    print("filename", "prelim" + ext)


def create_family_rows (fam, toolComp, gate_set):
  curr_list = Family.ls (fam)
  qlist = Family.lsqubits(fam)
  qlogs =  list (map(lambda x: log_from_bench(x, toolComp, gate_set), curr_list))
  llogs =  list (map (lambda x: log_from_bench(x, Tool.lopt, gate_set), curr_list))
  tab = []
  # LATEX IT
  qpcts = []
  lpcts = []
  ratsizes = []
  ratios = []
  fam_name = "\multirow{%d}{*}{%s}"%(len(curr_list), Family.name(fam))
  for (bn, q, l) in zip(qlist, qlogs, llogs):
    (l0, lf, gtl, _) = cols_from_log(l)
    if (gtl > 36000):
      (ltimes, lsizes, ltimes2, _) = split(l[0], l[1], 36000)
      (l0, lf, gtl) = (lsizes[0], int(lsizes[-1]), ltimes[-1])
    (qtimes, qsizes, qtimes2, _) = split(q[0], q[1], gtl)
    (q0, qf) = (int(qsizes[0]), int(qsizes[-1]))
    print("qfinal = ", qf)
    print("initial sizes = ", q0, l0)
    if q0 < 1000:
      continue
    elif (q0 == l0 or l0 == 0 or True):
      if len(qtimes2) == 0 and not (gtl >= 10000 and qtimes[-1] >= 10000):
        print("quartz hasn't run for this time", gtl, qtimes[-1])
        print(Colors.RED, bn, Colors.RESET)
      qpct = 0
      lpct = 0
      if q0 != lf:
        lpct = (round(float(q0-lf)/q0, 2) * 100)
      if q0 != qf:
        qpct = (round(float(q0-qf)/q0, 2) * 100)
      rat = round(float(1 + q0 - lf)/float(1 + q0 - qf), 2)
      # if qrate == 0:
      #   rat = 1.0
      # else:
      #   rat = round((lrate)/ (qrate), 2)
      # pctdiff = round((float(lf)/float(qf)) - 1.0, 2) * 100
      qpcts.append(qpct)
      lpcts.append(lpct)
      ratsize = round(float(qf)/float(lf), 2)
      ratios.append(rat)
      ratsizes.append(ratsize)
      q0lf = str(q0 -lf)
      if lf < qf:
        q0lf = "\\textbf{" + str(q0 - lf) + "}"
      tab.append([fam_name, bn, q0, str(q0 - qf) + " (%d\%%)" %(qpct), str(q0lf) + " (%d\%%)" %(lpct), str(rat) + "x", int(gtl)])
      fam_name = ""
    else:
      print(Colors.RED, bn, q0, l0, Colors.RESET)
  return tab

def create_new_table (fam_list, write, gate_set):
  # quartz_logs = list (map(lambda x: log_from_bench(x, Tool.quartz, gate_set), curr_list))
  # lopt_logs = list (map(lambda x: log_from_bench(x, Tool.lopt, gate_set), curr_list))
  # t = "Q^*"
  # headers = ["Name", "Input Size", t, "S", t+"/S", t, "S", "S/"+t,  "Time (s)"]
  def flatten(l):
    return [x for sublist in l for x in sublist]
  print("loop begine")
  tab = flatten(list(map (lambda x: create_family_rows(x, Tool.quartz, gate_set), fam_list)))

  tab_len = len(tab)
  if (len(tab) > 30 and write and False):
    h = int(tab_len/2)
    tab1 = tab[0:h]
    tab2 = tab[h:]
    ltab1 = tabulate(tab1, headers=headers, tablefmt="latex")
    ltab2 = tabulate(tab2, headers=headers, tablefmt="latex")
    ext = gate_set.value + ".%s.new.tex"%(Tool.extension())
    with open("prelim1" + ext, "w") as f:
      f.write(ltab1)
    with open("prelim2" + ext, "w") as f:
      f.write(ltab2)
    print("filenames", "prelim1" + ext, "prelim2" + ext)
  else:
    custom_header = (
        " &  &  & \\multicolumn{3}{c}{Number of optimizations} &  \\\\ \\cmidrule(lr){4-6} \n"
        "  Family & Qubits & Input Size & Q & S & S/Q & Time (s) \\\\ \n"
    )

    def find_line_position(input_string):
      first_newline_index = input_string.find('\n')
      return first_newline_index
    def mean(x):
      x = [abs(y)for y in x]
      return str(round(gmean(x), 2))
    ltab = tabulate(tab, tablefmt="latex_raw")
    pos = find_line_position(ltab)
    # insert header manually
    ltab = "\\begin{tabular}{ccccccc}" + "\n" + custom_header + ltab[pos:]
    ltab = ltab.replace('\hline', '')
    ltab = ltab.replace(r"\multirow", r"\midrule\multirow")
    # mean_row = "\\textbf{geomean} & & & & %s & \n"%(mean(ratios))
    last = "\end{tabular}"
    ltab = ltab.replace(last, "\midrule\n" + last)
    ext = gate_set.value + ".%s.tex"%(Tool.extension())
    print(ext)
    with open("prelim" + ext, "w") as f:
      f.write(ltab)
    print("filename", "prelim" + ext)



def create_swap_count(c, gate_set):
  bn = "hwb6"
  optl = qisk.get_swap_count(get_out_file(bn, Tool.lopt, gate_set), nam_basis)
  optq = qisk.get_swap_count(get_out_file(bn, Tool.quartz, gate_set), nam_basis)
  orig = qisk.get_swap_count(get_bench_file(bn, gate_set), nam_basis)
  return (orig, optq, optl)

def filter_fm (p, c):
  return [s for s in c if s.startswith(p) and s.endswith("from_python")]

# bench_list = sorted(listdir('benchmarks'))
# bench_list.remove('make')
# bench_list.remove('original')
# # bench_list.remove('vqe_n24')
# # bench_list.remove('grover_n11_from_python')
# # bench_list.remove('grover_n7_from_python')
# # bench_list.remove('barenco_tof_10')
# bench_list.remove('gf2^128_mult')
# bench_list.remove('gf2^163_mult')
# bench_list.remove('gf2^131_mult')
# bench_list.remove('gf2^64_mult')
def filter_bn (b, bn):
	filtered = [s for s in b if s.startswith(bn)]
	return filtered

# curr_list = ["adder_8"]
curr_list = []
curr_list += ["ham15-med", "ham15-high"]
name_list = curr_list.copy()
# [Family.hhl, Family.gf, Family.grover, Family.qftqis, Family.shor, Family.vqe]
for fam in [Family.hhl, Family.gf, Family.grover, Family.qftqis, Family.shor, Family.vqe]:
  curr_list += Family.ls (fam)
  name_list += Family.lslabels(fam)
fam_list = [Family.ham, Family.hhl, Family.gf, Family.grover, Family.qftqis, Family.shor, Family.vqe]
# fam_list = [Family.ham]
# run shor
# fam_list = [Family.ham, Family.hhl, Family.gf, Family.grover, Family.qftqis, Family.vqe]
# fam_list = [Family.grover]
  # if fam == Family.hhl:
  #   curr_list += ["hwb6"]
  #   name_list += ["hwb6"]
  # elif fam == Family.grover:
  #   curr_list += ["mod5_4"]
  #   name_list += ["mod5_4"]
# curr_list+=["shor_7_mod_15_n10_from_python"]
# name_list+=["qpe_n10"]
# print(curr_list)
# curr_list = ["qft_n48_from_qiskit", "qft_n64_from_qiskit", "qft_n80_from_qiskit"]
# curr_list += ["hhl_n7_from_python"]



# curr_list = ["qft_n48_from_qiskit", "qft_n64_from_qiskit", "qft_n80_from_qiskit"]
# curr_list += ["hhl_n7_from_python"]
# curr_list += ["ham15-high"]

# curr_list += ["ham15-med", "vqe_n12_from_python", "vqe_n20_from_python", "hhl_n9_from_python"]
# curr_list += ['gf2^16_mult', 'gf2^32_mult']
# curr_list += ["grover_n7_from_python", "grover_n9_from_python", "vqe_n16_from_python", "qft_n96_from_qiskit", "grover_n11_from_python"]
# print(log_from_bench("hhl_n7_from_python", Tool.queso, GateSet.nam))

# print(create_new_table(fam_list, True, GateSet.ibm))
# curr_list.remove("hhl_n11_from_python")
# # curr_list.remove("vqe_n24_from_python")
# name_list.remove("hhl\\_n11")
# # name_list.remove("vqe\\_n24")
# print(create_queso_tables(curr_list, name_list, True))
#
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

def fexists(bn):
  pref = "benchmarks/%s/%s"%(bn, bn) + (".lopt.%s.log"%(Tool.extension()))
  print(pref)
  return os.path.isfile(pref)

def retrieve_missing(fam_list):
  missing = []
  for fam in fam_list:
    vars = Family.ls (fam)
    for x in vars:
      if not fexists(x):
        missing.append(x)
  return missing

# lin_families = [Family.lin_grover, Family.lin_hhl, Family.lin_vqe, Family.lin_shor, Family.lin_qftqis]
# lin_families = [Family.lin_hhl]
# # plot_times(lin_families, GateSet.nam)
# print(log_from_bench("grover_n9_from_python", Tool.queso, GateSet.nam))

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

# curr_list = ['qaoa_n10_p4' ,'qaoa_n12_p4' ,'qaoa_n14_p4' ,'qaoa_n16_p4' ,'qaoa_n20_p4' ,'qaoa_n22_p4' ,'qaoa_n8_p4' ,'qaoa_n24_p4' ,'qaoa_n26_p4' ,'qaoa_n28_p4' ,'qaoa_n30_p4' ,'qaoa_n4_p4' ,'qaoa_n18_p4' ,'qaoa_n6_p4']
# print(plot_size(curr_list, GateSet.clifft, "qaoa"))
# print(fam_list[0].ls())
# nwq_names=["nwq_binary_welded_tree_n17", "nwq_binary_welded_tree_n21", "nwq_boolean_satisfaction_n28", "nwq_boolean_satisfaction_n30", "nwq_boolean_satisfaction_n32", "nwq_boolean_satisfaction_n34",  "nwq_multiplier_n200", "nwq_multiplier_n300", "nwq_multiplier_n400",  "nwq_square_root_n42", "nwq_square_root_n54", "nwq_square_root_n60",  "nwq_statevector_n5",  "nwq_statevector_n6", "nwq_statevector_n7", "nwq_statevector_n8", "nwq_vqc_n120", "nwq_vqc_n15", "nwq_vqc_n240", "nwq_vqc_n30", "nwq_vqc_n60"]
nwq_names_bwt=["nwq_binary_welded_tree_n17", "nwq_binary_welded_tree_n21","nwq_binary_welded_tree_n25","nwq_binary_welded_tree_n29"]
nwq_names_boolsat=["nwq_boolean_satisfaction_n28", "nwq_boolean_satisfaction_n30", "nwq_boolean_satisfaction_n32", "nwq_boolean_satisfaction_n34"]
nwq_names_sqrt=["nwq_square_root_n42", "nwq_square_root_n48", "nwq_square_root_n54", "nwq_square_root_n60"]
nwq_names_statevec=["nwq_statevector_n5",  "nwq_statevector_n6", "nwq_statevector_n7", "nwq_statevector_n8"]

configs=[(Tool.quartz, GateSet.nam),(Tool.queso, GateSet.nam), (Tool.voqc, GateSet.nam), (Tool.lopt_voqc, GateSet.nam)]
# configs=[(Tool.queso, GateSet.nam)]
# configs=[(Tool.quartz, GateSet.ibm), (Tool.queso, GateSet.ibm),(Tool.lopt_quartz, GateSet.ibm)]
fam_list = [ Family.grover,Family.hhl,Family.shor, Family.vqe]

def write_time_table (fam_rows):
  custom_header = (
    " &  &  & \\multicolumn{3}{c}{Number of optimizations} &  \\\\ \\cmidrule(lr){4-6} \n"
    "  Family & Qubits & Input Size & Q & S & S/Q & Time (s) \\\\ \n"
  )

  def find_line_position(input_string):
    first_newline_index = input_string.find('\n')
    return first_newline_index
  def mean(x):
    x = [abs(y)for y in x]
    return str(round(gmean(x), 2))
  ltab = tabulate(tab, tablefmt="latex_raw")
  pos = find_line_position(ltab)
  # insert header manually
  ltab = "\\begin{tabular}{ccccccc}" + "\n" + custom_header + ltab[pos:]
  ltab = ltab.replace('\hline', '')
  ltab = ltab.replace(r"\multirow", r"\midrule\multirow")
  # mean_row = "\\textbf{geomean} & & & & %s & \n"%(mean(ratios))
  last = "\end{tabular}"
  ltab = ltab.replace(last, "\midrule\n" + last)
  ext = gate_set.value + ".%s.tex"%(Tool.extension())
  print(ext)
  with open("prelim" + ext, "w") as f:
    f.write(ltab)
  print("filename", "prelim" + ext)



def call_time_extract (bn):
  call_times = []
  filename = "benchmarks/%s/%s.voqc.trash.out"%(bn, bn)
  with open(filename, 'r') as file:
    for line in file:
      match =  re.search(r"call time = (\d+\.\d+)", line)
      if match:
          # Extract the time and add it to the total
          time = float(match.group(1))
          call_times.append(time)

  return statistics.mean(call_times)


def num_calls (bn):
  filename = "benchmarks/%s/%s.convergence.voqc.trash.out"%(bn, bn)
  count = 0
  with open(filename, 'r') as file:
    for line in file:
      match =  re.search(r"call time = (\d+\.\d+)", line)
      if match:
        count+=1

  return count
# fam_list = [Family.hhl]
import csv


def generate_call_time():
  name_list=[]
  name_list+=nwq_names_boolsat
  name_list+=nwq_names_bwt
  name_list+=fam_list[0].ls()
  name_list+=fam_list[1].ls()
  name_list+=fam_list[2].ls()
  name_list+=nwq_names_sqrt
  name_list+=nwq_names_statevec
  name_list+=fam_list[3].ls()
  call_time_list=[]
  for name in name_list:
    try:
      call_time_list.append(num_calls(name))
    except:
      print("failed", name)
  print("\n".join([str(x) for x in call_time_list]))

def generate_table_pengyu ():
  my_csv=[["-" for _ in range(17)] for _ in range(35)]
  name_list=[]
  name_list+=nwq_names_boolsat
  name_list+=nwq_names_bwt
  name_list+=fam_list[0].ls()
  name_list+=fam_list[1].ls()
  name_list+=fam_list[2].ls()
  name_list+=nwq_names_sqrt
  name_list+=nwq_names_statevec
  name_list+=fam_list[3].ls()


  for i,name in enumerate(name_list):
    my_csv[i][0]=name
    print(name)
  for i,name in enumerate(name_list):
    log=log_from_bench(name, configs[3][0], configs[3][1])
    my_csv[i][1]=log[1][0]


    for j,config in enumerate(configs):
      offset=2*j+2
      if j>3:
        offset+=1
      # log=log_from_bench(name, config[0], config[1])
      # try:
      log=log_from_bench(name, config[0], config[1])
      print(name, log)
        # try:
      if log[2] is not None:
        my_csv[i][offset]=log[2]
      if log[3] is not None:
        my_csv[i][offset+1]=log[3]
      #   except:
      #     pass
      # except:
      #   pass


  with open('pengyu_result.csv', 'w', newline='') as file:
    writer = csv.writer(file)
    writer.writerows(my_csv)


def generate_table_mingkuan():
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
  my_csv=[["-" for _ in range(17)] for _ in range(len(curr_list))]
  for i,name in enumerate(curr_list):
    my_csv[i][0]=name
    print(name)
  configs=[(Tool.lopt_pyzx, GateSet.clifft), (Tool.pyzx, GateSet.clifft)]
  for i,name in enumerate(curr_list):
    log=log_from_bench(name, configs[1][0], configs[1][1])
    my_csv[i][1]=log[1][0] # t count


    for j,config in enumerate(configs):
      offset=2*j+2
      # log=log_from_bench(name, config[0], config[1])
      # try:
      log=log_from_bench(name, config[0], config[1])
      print(name, log)
      # try:
      if log[2] is not None:
        my_csv[i][offset]=log[2]
      if log[3] is not None:
        my_csv[i][offset+1]=log[3]
      #   except:
      #     pass
      # except:
      #   pass


  with open('mingkuan_result.csv', 'w', newline='') as file:
    writer = csv.writer(file)
    writer.writerows(my_csv)


# generate_table_pengyu()
# generate_call_time()
generate_table_mingkuan()
