package kuplrg

// The definition of PDA
case class PDA(
  states: Set[State],
  symbols: Set[Symbol],
  alphabets: Set[Alphabet],
  trans: Map[(State, Option[Symbol], Alphabet), Set[(State, List[Alphabet])]],
  initState: State,
  initAlphabet: Alphabet,
  finalStates: Set[State],
) {
  // Configurations reachable from the initial configuration by one-step moves
  def reachableConfig(init: Config): Set[Config] =
    def aux(
      targets: List[Config],
      visited: Set[Config]
    ): Set[Config] = targets match
      case Nil => visited
      case config :: targets => aux(
        targets =
          val (q, w, xs) = config
          (
            eclose(q, xs).map { case (q, xs) => (q, w, xs) }
            ++ ((w, xs) match
              case (a <| w, x :: xs) => trans((q, Some(a), x)).map {
                case (q, ys) => (q, w, ys ++ xs)
              }
              case _ => Set()
            )
            -- visited
          ).toList ++ targets,
        visited = visited + config,
      )
    aux(List(init), Set())

  // The epsilon-closures for pairs of states and stacks
  def eclose(
    q: State,
    xs: List[Alphabet]
  ): Set[(State, List[Alphabet])] =
    def aux(
      targets: List[(State, List[Alphabet])],
      reachable: Set[(State, List[Alphabet])],
    ): Set[(State, List[Alphabet])] = targets match
      case Nil => reachable
      case (q, xs) :: targets => aux(
        (xs match
          case x :: xs => (trans(q, None, x).map {
            case (q, ys) => (q, ys ++ xs)
          } -- reachable).toList
          case _ => Nil
        ) ++ targets,
        reachable + ((q, xs)),
      )
    aux(List((q, xs)), Set())

  // Acceptance by final states
  def acceptByFinalState(word: Word): Boolean =
    val init: Config = (initState, word, List(initAlphabet))
    reachableConfig(init).exists(config => {
      val (q, w, xs) = config
      w.isEmpty && finalStates.contains(q)
    })

  // Acceptance by empty stacks
  def acceptByEmptyStack(word: Word): Boolean =
    val init: Config = (initState, word, List(initAlphabet))
    reachableConfig(init).exists(config => {
      val (q, w, xs) = config
      w.isEmpty && xs.isEmpty
    })

  // The language of the PDA by final states
  lazy val langByFinalStates: Lang = (symbols, acceptByFinalState)

  // The language of the PDA by empty stacks
  lazy val langByEmptyStacks: Lang = (symbols, acceptByEmptyStack)

  // A string form of the PDA
  lazy val stringForm: String =
    val simple = alphabets.forall(_ < 26)
    def getName(x: Alphabet): String =
      if (simple) (x + 'A').toChar.toString else s"X_{$x}"
    s"""
    |    - states: ${states.toList.sorted.mkString(", ")}
    |    - symbols: ${symbols.toList.sorted.mkString(", ")}
    |    - alphabets: ${alphabets.toList.sorted.map(getName).mkString(", ")}
    |    - initState: ${initState}
    |    - initAlphabet: ${getName(initAlphabet)}
    |    - finalStates: ${finalStates.toList.sorted.mkString(", ")}
    |    - trans: {
    |        ${(for {
      ((p, a, x), set) <- trans.toList.sortBy(_._1)
      (q, ys) <- set.toList.sortBy(_._1)
      aStr = a.fold("<e>")(" " + _ + " ")
      xStr = getName(x)
      ysStr = ys.map(getName).mkString
    } yield f"$p -$aStr-> $q [$xStr -> $ysStr]").mkString("\n        ")}
    |      }""".stripMargin

  // Dump the DPA
  def dump: Unit =
    show(s"* A PDA is dumped:")
    println(s"  ${green("* String form:")} ${stringForm}")
    println(s"  ${green("* Scala object:")} ${this}")
}
object PDA {
  def apply(
    initState: State,
    initAlphabet: Alphabet,
    finalStates: Set[State],
    transSeq: ((State, Option[Symbol], Alphabet), (State, List[Alphabet]))*,
  ): PDA =
    val states = for { ((p, _, _), (q, _)) <- transSeq.toSet; q <- Set(p, q) } yield q
    val symbols = for { ((_, opt, _), _) <- transSeq.toSet; a <- opt } yield a
    val alphabets = for { ((_, _, x), (_, ys)) <- transSeq.toSet; x <- x :: ys } yield x
    val map = transSeq.groupMap(_._1)(_._2).map(_ -> _.toSet)
    val trans = (for {
      q <- states
      a <- symbols.map(Some(_)) + None
      x <- alphabets
    } yield (q, a, x) -> map.getOrElse((q, a, x), Set())).toMap
    PDA(
      states = states,
      symbols = symbols,
      alphabets = alphabets,
      trans = trans,
      initState = initState,
      initAlphabet = initAlphabet,
      finalStates = finalStates,
    )
}
