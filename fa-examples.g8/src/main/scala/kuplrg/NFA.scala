package kuplrg

// The definition of NFA
case class NFA(
  states: Set[State],
  symbols: Set[Symbol],
  trans: Map[(State, Symbol), Set[State]],
  initState: State,
  finalStates: Set[State],
) extends FA {

  // The extended transition function of NFA
  def extTrans(q: State, w: Word): Set[State] = w match
    case "" => Set(q)
    case a <| x => trans(q, a).flatMap(extTrans(_, x))

  // The acceptance of a word by NFA
  def accept(w: Word): Boolean =
    val curStates: Set[State] = extTrans(initState, w)
    curStates.intersect(finalStates).nonEmpty
}
