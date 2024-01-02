package kuplrg

// The definition of DFA
case class DFA(
  states: Set[State],
  symbols: Set[Symbol],
  trans: Map[(State, Symbol), State],
  initState: State,
  finalStates: Set[State],
) extends FA {

  // The extended transition function of DFA
  def extTrans(q: State, w: Word): State = w match
    case "" => q
    case a <| x => extTrans(trans(q, a), x)

  // The acceptance of a word by DFA
  def accept(w: Word): Boolean =
    val curSt: State = extTrans(initState, w)
    finalStates.contains(curSt)
}
