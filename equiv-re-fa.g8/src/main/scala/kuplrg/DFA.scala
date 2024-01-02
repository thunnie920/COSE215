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

  // The normalized DFA with states from 1 to n
  def normalized: DFA =
    val stateList = states.toList.sorted
    val n = stateList.length
    val map = stateList.zipWithIndex.map((q, i) => q -> (i + 1)).toMap
    DFA(
      states = (1 to n).toSet,
      symbols = symbols,
      trans = trans.map { case ((q, a), r) => ((q), a) -> map(r) },
      initState = map(initState),
      finalStates = finalStates.map(map),
    )
}
object DFA {
  def apply(
    numSt: Int,
    symbols: String,
    trans: Int,
    finalStates: Int,
  ): DFA =
    val stateList = (1 to numSt).toList
    val symbolList = symbols.toList.sorted
    DFA(
      states = stateList.toSet,
      symbols = symbolList.toSet,
      trans =
        val pairs = stateList.flatMap(q => symbolList.map((q, _)))
        val (map, _) = pairs.foldLeft((Map[(State, Symbol), State](), trans)) {
          case ((m, t), (q, a)) => (m + ((q, a) -> (t % numSt + 1)), t / numSt)
        }
        map,
      initState = 1,
      finalStates =
        val (set, _) = stateList.foldLeft((Set[State](), finalStates)) {
          case ((s, f), q) => (if (f % 2 == 1) s + q else s, f / 2)
        }
        set,
    )
}
