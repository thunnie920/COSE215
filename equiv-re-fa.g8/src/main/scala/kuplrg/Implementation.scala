package kuplrg

object Implementation extends Template {

  // Convert a regular expression to a epsilon-NFA
  def re2enfa(re: RE): ENFA =
    val SimpleENFA(from, trans, to) = re2senfa(re, 1)
    val states = (from to to).toSet
    val symbols = trans.flatMap((_, aOpt, _) => aOpt)
    val map = trans.groupMap((i, aOpt, _) => (i, aOpt))((_, _, j) => j)
    val enfaTrans = states
      .flatMap(q => symbols.map(a => q -> Option(a)) + (q -> None))
      .map(pair => pair -> map.getOrElse(pair, Set()))
      .toMap
    ENFA(
      states = states,
      symbols = symbols,
      trans = enfaTrans,
      initState = from,
      finalStates = Set(to),
    )

  // Convert a regular expression `re` to a simplified epsilon-NFA with an
  // initial state `i`.
  def re2senfa(re: RE, i: State): SimpleENFA = re match
    case REEmpty() => SimpleENFA(
      from = i, trans = Set(),                         to = i + 1,
    )
    case REEpsilon() => SimpleENFA(
      from = i, trans = Set((i, None, i + 1)),         to = i + 1,
    )
    case RESymbol(symbol) => SimpleENFA(
      from = i, trans = Set((i, Some(symbol), i + 1)), to = i + 1,
    )
    case REUnion(re1, re2) => 
      val SimpleENFA(_, trans1, j) = re2senfa(re1, i + 1)
      val SimpleENFA(_, trans2, k) = re2senfa(re2, j + 1)
      SimpleENFA(
        from  = i,
        trans = trans1 ++ trans2 ++ Set(
          (i, None, i + 1), (i, None, j + 1),
          (j, None, k + 1), (k, None, k + 1),
        ),
        to    = k + 1,
      )
    case REConcat(re1, re2) => 
      val SimpleENFA(_, trans1, j) = re2senfa(re1, i)
      val SimpleENFA(_, trans2, k) = re2senfa(re2, j + 1)
      SimpleENFA(
        from  = i,
        trans = trans1 ++ trans2 ++ Set ((j, None, j + 1)),
        to    = k ,
      )
    case REStar(re) =>
      val SimpleENFA(_, trans, j) = re2senfa(re, i + 1)
      SimpleENFA(
        from  = i,
        trans = trans ++ Set(
          (i, None, i + 1), (j, None, i + 1),
          (j, None, j + 1), (i, None, j + 1),
        ),
        to    = j + 1,
      )
    case REParen(re) =>
      val SimpleENFA(_, trans, j) = re2senfa(re, i)
      SimpleENFA(
        from  = i,
        trans = trans,
        to    = j,
      )

  // Convert a DFA to a regular expression
  def dfa2re(givenDFA: DFA, debug: Boolean = false): RE = {
    val dfa = givenDFA.normalized

    // Show details in the conversion from a DFA to a regular expression
    if (debug) {
      show("* Details in the conversion from a DFA to a regular expression:")
      val n = dfa.states.size
      for (k <- 0 to n; i <- 1 to n; j <- 1 to n) {
        val re = reForPaths(dfa)(i, j, k)
        println(s"  - ($i, $j, $k) -> ${re.stringForm}")
      }
    }
    val n = dfa.states.size
    val startState = dfa.initState
    val finalStates = dfa.finalStates
    val res = finalStates.foldLeft(REEmpty(): RE) { (acc, f) =>
      REUnion(acc, reForPaths(dfa)(startState, f, n))
    }
    res   
  }
  // A regular expression accepting paths from `i` to `j` with intermediate
  // states bounded by `k` in a given DFA `dfa`. Assume that the given DFA
  // `dfa` is already normalized (i.e., the states of DFA are 1, 2, ..., n).

  def findTransitionSymbol(dfa: DFA)(i: State, j: State): Set[Symbol] = {
  dfa.symbols.filter { s =>
    dfa.trans.contains((i, s)) && dfa.trans((i, s)) == j
  }.toSet
}

  def reForPaths(dfa: DFA)(i: State, j: State, k: State): RE = k match {
    case 0 =>
      if (i == j) {
        REUnion(REEmpty(), findTransitionSymbol(dfa)(i, j).map(RESymbol).reduceLeftOption(REUnion).getOrElse(REEmpty()))
      } else {
        findTransitionSymbol(dfa)(i, j).map(RESymbol).reduceLeftOption(REUnion).getOrElse(REEmpty())
      }
    case _ =>
      val re  = reForPaths(dfa)(k, k, k - 1)
      val re1 = reForPaths(dfa)(i, j, k - 1)
      val re2 = reForPaths(dfa)(i, k, k - 1)
      val re3 = reForPaths(dfa)(k, j, k - 1)
      val re4 = REConcat(re2, REStar(re))
      val re5 = REConcat(re4, re3)
      REUnion(re1, re5)
    }
  }
