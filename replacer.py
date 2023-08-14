import re
import math
import ast
import os
def convert_param_to_numeric(param_str):
    # Replace "pi" with its numerical value
    param_str = param_str.replace('pi', str(math.pi))

    # Safely evaluate the expression and convert to numeric value
    return ast.literal_eval(param_str)

def file_contents (f):
    with open(f, 'r') as file:
        contents = file.read()
    return contents

def replace_pi_with_value(qasm_file):
    # Read the QASM file
    qasm_code = file_contents(qasm_file)
    pattern = r'(?P<gate>[crzRZ]+)\((?P<params>pi\s*\*\s*[-+]?\d*\.?\d+)\)'

    # Find all matches in the QASM code
    matches = re.finditer(pattern, qasm_code)
    # Loop through each match and replace π with its numerical value
    for match in matches:
        gate = match.group('gate')
        params = match.group('params')
        angle = float(params.strip('()').replace('pi', '').replace('*', '')) * math.pi
        qasm_code = qasm_code.replace(match.group(), f"{gate}({angle})")

    # Write the modified QASM code back to the file
    with open(qasm_file+ ".new", 'w') as file:
        file.write(qasm_code)


def replace_bench(bn):
    qasm_file_path = 'benchmarks/%s/%s.qasm.preprocessed'%(bn, bn)
    if os.path.exists(qasm_file_path + ".new"):
        print(qasm_file_path+".new", "already exists!")
        return
    print("doing, ", bn)
    replace_pi_with_value(qasm_file_path)
    print("done, ", bn)

# nned to do hhl_n11
curr_list = ['ham15-med', 'ham15-high', 'hhl_n7_from_python', 'hhl_n9_from_python', 'gf2^16_mult', 'gf2^32_mult', 'grover_n7_from_python', 'grover_n9_from_python', 'grover_n11_from_python', 'grover_n15_from_python', 'qft_n48_from_qiskit', 'qft_n64_from_qiskit', 'qft_n80_from_qiskit', 'qft_n96_from_qiskit', 'shor_7_mod_15_n8_from_python', 'shor_7_mod_15_n10_from_python', 'vqe_n12_from_python', 'vqe_n16_from_python', 'vqe_n20_from_python', 'vqe_n24_from_python']
print(list(map(lambda x: replace_bench(x), curr_list)))


def extract_time_from_log (bn):
    # grover_n7_from_python.queso.trash.out
    log_file_path = 'benchmarks/%s/%s.queso.trash.out'%(bn, bn)
    log_text = file_contents(log_file_path)
    # Define the regular expression pattern to match "optimized in x seconds"
    pattern = r"optimized in (\d+(?:\.\d+)?) seconds"

    # Find all matches of the pattern in the log_text
    matches = re.findall(pattern, log_text)

    # Convert the matched strings to float and return as a list
    return (bn, sum([float(match) for match in matches]))

# curr_list = ["vqe_n12_from_python", "vqe_n8_from_python", "qft_n64_from_qiskit", "qft_n48_from_qiskit"]
# print(list(map(lambda x: extract_time_from_log(x), curr_list)))

def create_table (contents):
    rows = contents.split('\n')
    entries = list(map(lambda x : [y.strip() for y in x.split(",")], rows))
    def rate (e):
        (qopt, lopt, t, sz) = (float(e[1]), float(e[2]), float(e[3]), int(e[4]))
        return [e[0], sz, qopt, lopt, qopt/t, lopt/t, lopt/qopt, t]
    tab = list(map (lambda e : rate(e), entries))
    custom_header = (
        " &   \\multicolumn{3}{c}{Circuit Size} &  \\multicolumn{3}{c}{Optimization Rate} &  \\\\ \\cmidrule(lr){2-4} \\cmidrule(lr){5-7}\n"
        "  Name & Original & Quartz & Lopt & Quartz &   Lopt & Ratio  & Time (s) \\\\ \n"
    )

    def find_line_position(input_string):
      first_newline_index = input_string.find('\n')
      return first_newline_index
    ltab = tabulate(tab, tablefmt="latex_raw")
    pos = find_line_position(ltab)
    # insert header manually
    ltab = "\\begin{tabular}{rrrrrrrr}" + "\n" + custom_header + ltab[pos:]
    ltab = ltab.replace('hline', 'midrule')
    with open("queso.tex", "w") as f:
      f.write(ltab)
    print("filename", "queso" + ext)

# create_table (file_contents("queso_results.csv"))
