from enum import Enum

class Family (Enum):
  grover = ["7",  "9", "11", "15"]
  hhl = ["7",  "9", "11"] #13
  qft = ["8",  "16", "24", "30"]
  # qftqis = ["8", "16", "32", "48", "64", "80", "96", "128"]
  qftqis = ["48", "64", "80", "96"]
  shor = ["8", "10"]
  # vqe = ["6", "8", "16", "24", "28"]
  vqe = ["12", "16", "20", "24"]
  # gf = ["8","16", "32"]
  gf = ["16", "32"]

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
    else:
      print("NOT IMPLEMENTED")

  def file (fam, var):
    if fam == Family.grover or fam == Family.lin_grover:
      return "grover_n%s_from_python"%(var)
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
    if fam == Family.arithmetic or fam == Family.lin_arithmetic:
      return var
    if fam == Family.gf or fam == Family.lin_gf:
      return "gf2^%s_mult"%(var)
    else:
      print("NOT IMPLEMENTED")

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
    if fam == Family.gf or fam == Family.lin_gf:
      return "gf2\^{}%s\_mult"%(var)
    else:
      print("NOT IMPLEMENTED")

  def ls(fam):
    return list(map(lambda x : Family.file(fam, x), fam.value))

  def lslabels(fam):
    return list(map(lambda x : Family.labels(fam, x), fam.value))