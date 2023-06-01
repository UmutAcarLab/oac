import matplotlib.pyplot as plt
import glob

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

def read_quartz_log (f):
  with open (f) as file:
    return file.read().split('\n')[-3]

def read_local_log (f):
  with open (f) as file:
    return file.read()

def plot_bench (bn, greedy):
  if greedy:
    suffix = '.qasm.greedy.log'
  else:
    suffix = '.qasm.search.log'
  bs = bn + suffix
  (dq, dlopt) = (read_quartz_log("logs/quartz/" + bs), read_local_log ("logs/lopt/" + bs))
  (tq, szq) = parse_data (dq)
  print(tq, szq)
  if (len(tq) >= 2):
    plt.plot(tq, szq, marker = 's', label='Quartz', color = 'blue')
    plt.plot(tq[-1], szq[-1], marker = 's', color = 'blue')
    plt.axvline(x=tq[-2], color='red', linestyle='--')
  (tl, szl) = parse_data (dlopt)
  szl[0] = szq[0]
  print(tl, szl)
  if (len(tl) >= 2):
    plt.plot(tl, szl, marker = 'o', label='Local optimizer', color = 'green')
    plt.axvline(x=tl[-2], color='red', linestyle='--')
  plt.xlabel('Time')
  plt.ylabel('Circuit Size')
  plt.legend()
  if greedy:
    plt.title('%s: Circuit Size vs. Time (greedy)'%(bn.capitalize()))
    plt.savefig("plots/%s.greedy.png"%(bn), dpi=300)
  else:
    plt.title('%s: Circuit Size vs. Time (search)'%(bn.capitalize()))
    plt.savefig("plots/%s.search.png"%(bn), dpi=300)

plot_bench ("vbe_adder_3", False)
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
  if greedy:
    suffix = '.qasm.greedy.log'
  else:
    suffix = '.qasm.search.log'
  files = log_files_and_names("logs/lopt/", suffix)
  def report_from_file (f, bn):
    d = read_local_log (f)
    (times, sizes) = parse_data(d)
    return (bn, sizes[0], sizes[-1], times [-1])
  results = list(map (lambda f: report_from_file(f[0], f[1]), files))
  results = sorted(results, key=lambda x: x[0])
  csv_printer(results)

def parse_quartz (greedy):
  suffix = ""
  if greedy:
    suffix = '.qasm.greedy.log'
  else:
    suffix = '.qasm.search.log'
  def report_from_file (f, bn):
    d = read_quartz_log (f)
    (times, sizes) = parse_data(d)
    if len(sizes) < 2:
      return (bn, None, None, None)
    return (bn, sizes[0], sizes[-1], times [-1])
  files = log_files_and_names("logs/quartz/", suffix)
  results = list(map (lambda f: report_from_file(f[0], f[1]), files))
  results = sorted(results, key=lambda x: x[0])
  csv_printer(results)

# parse_quartz(False)
# parse_lopt(False)
# Plotting the graph


# Labeling the axes and providing a title


# Displaying the graph

