import matplotlib.pyplot as plt

def parse_data (d):
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

def read_file (f):
  with open (f) as file:
    return file.read()

def plot_from_files (fq, flopt, bench_name):
  (dq, dlopt) = (read_file (fq), read_file (flopt))
  (tq, szq) = parse_data (dq)
  plt.plot(tq, szq, marker = 's', label='Quartz', color = 'blue')
  plt.axvline(x=tq[-2], color='red', linestyle='--')
  (tl, szl) = parse_data (dlopt)
  plt.plot(tl, szl, marker = 'o', label='Local optimizer', color = 'green')
  plt.axvline(x=tl[-2], color='red', linestyle='--')
  plt.xlabel('Time')
  plt.ylabel('Circuit Size')
  plt.title('Circuit Size vs. Time')
  plt.legend()
  plt.savefig(bench_name+".png")

plot_from_files ("logs/vbe_adder3_quartz_search", "logs/vbe_adder3_local_search", "vbe_adder3")
# Sample data

# Plotting the graph


# Labeling the axes and providing a title


# Displaying the graph

