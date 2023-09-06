import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import org.jgrapht.graph.DirectedMultigraph;
import org.apache.commons.math3.util.Pair;
import java.io.IOException;

public class Wrapper {


  // These are default options i took from Applier
  static Integer maxSymbQubits = 7;
  static Integer maxRuleQubits = -1;
  static Integer itersBeforePrune = -1;
  static Boolean preserveMapping = false;
  static String optObj = "total";
  static Integer maxQueueSize = 8000;
  static Boolean pruneProportional = false;
  static Integer maxSymbSize = 10;
  static Boolean useSizePreservingSymbRules = false;

  private List<Pair<DirectedMultigraph<Node,Edge>,String>> allRules;
  private List<Pair<DirectedMultigraph<Node,Edge>,String>> greedyRules;
  private List<String> allSymbRules;
  private List<String> greedySymbRules;

  public Wrapper (String rulesFile, String symbRulesFile) throws IOException {
    Applier applier = new Applier(new Random(), maxSymbQubits);
    this.allRules = applier.getRules(rulesFile, false, maxRuleQubits, preserveMapping, optObj);
    this.greedyRules = applier.getRules(rulesFile, true, maxRuleQubits, preserveMapping, optObj);
    this.allSymbRules = applier.getSymbRules(symbRulesFile, false, maxRuleQubits, preserveMapping);
    this.greedySymbRules = applier.getSymbRules(symbRulesFile, true, maxRuleQubits, preserveMapping);
  }

  private Pair<String, String> parseHeader (String content) {
    String header = "";
    header += content.substring(0, content.indexOf("\n") + 1);
    content = content.substring(content.indexOf("\n") + 1);
    header += content.substring(0, content.indexOf("\n") + 1);
    content = content.substring(content.indexOf("\n") + 1);
    header += content.substring(0, content.indexOf("\n") + 1);
    content = content.substring(content.indexOf("\n") + 1);
    content = content.replace("qubits[", "q");
    content = content.replace("q[", "q");
    content = content.replace(", ", ",");
    content = content.replace("];", "; ");
    content = content.replace("],", ", ");
    content = content.replace("\n", "");
    content = content.trim();
    return new Pair<> (header, content);
  }

  public String optimize (String cqasm, boolean applyAll, int timeout) throws IOException {
    Applier applier = new Applier (new Random(), maxSymbQubits);
    Pair<String, String> hc = parseHeader(cqasm);
    String header = hc.getFirst();
    String content = hc.getSecond();
    List<Pair<String, Integer>> rulesApplied = new ArrayList<>();
    DirectedMultigraph<Node, Edge> dag = applier.qasmToDag(content);
    OptCircuit optimized = applier.optimizeWithRules(new OptCircuit(dag, rulesApplied, System.nanoTime()), false, "trash",
        timeout, maxQueueSize, this.allRules, this.allSymbRules, itersBeforePrune, pruneProportional, maxSymbQubits,
        maxSymbSize, preserveMapping, maxRuleQubits, optObj);
    // OptCircuit optimized = applier.optimize(new OptCircuit(dag, rulesApplied, System.nanoTime()), true, "trash",
    //     timeout, maxQueueSize, "rules_q3_s6_nam.txt", "rules_q3_s3_nam_symb.txt", itersBeforePrune, pruneProportional, maxSymbQubits,
    //     maxSymbSize, true, true, preserveMapping, maxRuleQubits, optObj);

    String qasm = applier.dagToQasm(optimized.getCircuit());
    qasm = qasm.replace("q", "q[");
    qasm = qasm.replace(";", "];");
    qasm = qasm.replace(", ", "], ");
    qasm = header + qasm;
    return qasm;
  }
}
