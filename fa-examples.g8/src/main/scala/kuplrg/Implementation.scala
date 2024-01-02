package kuplrg

object Implementation extends Template {

  // Assign any automaton you want to dump into the automata viewer
  def dumpTarget: FA = dfa_w00

  def dfa_w00: DFA = DFA(
    states = Set(0, 1, 2),
    symbols = Set('0', '1'),
    trans = Map(
      (0, '0') -> 1,
      (0, '1') -> 0,
      (1, '0') -> 2,
      (1, '1') -> 0,
      (2, '0') -> 2,
      (2, '1') -> 0,
    ),
    initState = 0,
    finalStates = Set(2),
  )

  def dfa_a_star_b: DFA = DFA(
    states = ???,
    symbols = ???,
    trans = ???,
    initState = ???,
    finalStates = ???,
  )

  def dfa_div_3: DFA = DFA(
    states = ???,
    symbols = ???,
    trans = ???,
    initState = ???,
    finalStates = ???,
  )

  def dfa_subseq_011: DFA = DFA(
    states = ???,
    symbols = ???,
    trans = ???,
    initState = ???,
    finalStates = ???,
  )

  def dfa_even_0_1: DFA = DFA(
    states = ???,
    symbols = ???,
    trans = ???,
    initState = ???,
    finalStates = ???,
  )

  def nfa_least_two_0: NFA = NFA(
    states = ???,
    symbols = ???,
    trans = ???,
    initState = ???,
    finalStates = ???,
  )

  def nfa_two_0: NFA = NFA(
    states = ???,
    symbols = ???,
    trans = ???,
    initState = ???,
    finalStates = ???,
  )

  def nfa_substr_000: NFA = NFA(
    states = ???,
    symbols = ???,
    trans = ???,
    initState = ???,
    finalStates = ???,
  )

  def enfa_ab_plus: ENFA = ENFA(
    states = ???,
    symbols = ???,
    trans = ???,
    initState = ???,
    finalStates = ???,
  )

  def enfa_same_digits: ENFA = ENFA(
    states = ???,
    symbols = ???,
    trans = ???,
    initState = ???,
    finalStates = ???,
  )

  def enfa_aibjck: ENFA = ENFA(
    states = ???,
    symbols = ???,
    trans = ???,
    initState = ???,
    finalStates = ???,
  )
}
