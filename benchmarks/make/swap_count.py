from qiskit.providers.fake_provider import FakePrague
from qiskit import QuantumCircuit, transpile

def get_swap_count(
        qasm_file_path,
        original_basis_gates,
        backend=FakePrague()
):
    """
    Returns the swap count for the given qasm file.
    1. convert the qasm file into a qiskit circuit
    2. transpile the circuit onto the hardware topology, using the original basis gates + swap
    3. count the number of swaps after transpiling
    This SWAP count describes the number of SWAPs that are needed to map the circuit onto the hardware topology.
    If we get both swap counts for with or without the compiler,
    the difference describes whether the compiler provides advantages even if the hardware topology is considered.

    Args:
        qasm_file_path (str): Path to the qasm file w.r.t. the current working directory.
        original_basis_gates (list): List of basis gates used in the QASM file so that the transpiler does not add complexity.
        backend (qiskit.FakeProviderForBackendV2): Backend to transpile the circuit onto. (Default: FakePrague 33 qubits)

    Returns:
        int: Swap count.
    """
    circ = QuantumCircuit.from_qasm_file(qasm_file_path)
    basis_gates = None
    if 'swap' in original_basis_gates:
        print('WARNING: SWAP is already in the basis gates. This might lead to unexpected results.')
    else:
        basis_gates = original_basis_gates + ['swap']
    transpiled_circ = transpile(
        circ,
        backend=backend,
        basis_gates=basis_gates,
        seed_transpiler=722,
        optimization_level=0,
    )
    counts = transpiled_circ.count_ops()
    return counts.get('swap', 0)

if __name__ == '__main__':
    qasm_file_path = '/home/dl2276/AllProjects/quicr/optimizer/benchmarks/qaoa_n24_from_python/qaoa_n24_from_python.qasm'
    basis_gates = ['x', 'h', 'rz', 'add', 'cx']
    print(get_swap_count(qasm_file_path, basis_gates))