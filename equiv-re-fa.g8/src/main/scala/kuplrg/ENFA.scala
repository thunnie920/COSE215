package kuplrg

// The definition of epsilon-NFA
case class ENFA(
  states: Set[State],
  symbols: Set[Symbol],
  trans: Map[(State, Option[Symbol]), Set[State]],
  initState: State,
  finalStates: Set[State],
) extends FA {

  // The definitions of epsilon-closures
  def eclose(q: State): Set[State] =
    def aux(targets: List[State], visited: Set[State]): Set[State] = targets match
      case Nil => visited
      case p :: targets => aux(
        targets = (trans((p, None)) -- visited).toList ++ targets,
        visited = visited + p,
      )
    aux(List(q), Set())

  // The extended transition function of epsilon-NFA
  def extTrans(q: State, w: Word): Set[State] = w match
    case "" => eclose(q)
    case a <| x => eclose(q)
      .flatMap(trans(_, Some(a)))
      .flatMap(extTrans(_, x))

  // The acceptance of a word by epsilon-NFA
  def accept(w: Word): Boolean =
    val curStates: Set[State] = extTrans(initState, w)
    curStates.intersect(finalStates).nonEmpty
}
