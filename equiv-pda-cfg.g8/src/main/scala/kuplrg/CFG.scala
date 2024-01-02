package kuplrg

import scala.util.parsing.combinator.*

// The definition of context-free grammars
case class CFG(
  variables: Set[Variable],
  symbols: Set[Symbol],
  start: Variable,
  productions: Set[(Variable, List[Variable | Symbol])],
) {
  // A string form of the CFG
  lazy val stringForm: String = productions
    .groupMap(_._1)(_._2)
    .toList
    .sortBy { case (x, _) => (x != start, x) }
    .map((nt, seqs) => s"${CFG.ntStr(nt)} -> " + seqs.map(seq =>
      if (seq.isEmpty) "<e>" else seq.map {
        case nt: Variable => CFG.ntStr(nt)
        case a: Symbol => a.toString
      }.mkString(" ")
    ).toList.sorted.mkString(" | ") + " ;;").mkString("\n")

  // Dump the CFG
  def dump: Unit =
    show(s"* A CFG is dumped:")
    println(s"  ${green("* String form:")} ${
      stringForm.split("\n").map("\n    " + _).mkString
    }")
    println(s"  ${green("* Scala object:")} ${this}")
}
object CFG extends RegexParsers with PackratParsers {
  def apply(cfgStr: String): CFG = parseAll(cfg, cfgStr).get
  type P[T] = Parser[T]
  type PP[T] = PackratParser[T]
  // A context-free grammar (CFG) parser
  given cfg: PP[CFG] =
    lazy val nt: PP[Variable] = "'[A-Z]+".r ^^ { case s => getId(s.drop(1)) }
    lazy val t: Parser[Symbol] = "[-+*{}()0-9a-z]".r ^^ { _.charAt(0) }
    lazy val sym: Parser[Variable | Symbol] = nt | t
    lazy val alt = rep1(sym) | "<e>" ^^^ Nil
    lazy val prod = (nt <~ "->") ~ rep1sep(alt, "|") <~ ";;" ^^ {
      case nt ~ alts => nt -> alts.toSet
    }
    rep(prod) ^^ { prodSeq => prodSeq match
      case Nil => CFG(Set(0), Set(), 0, Set())
      case (start, _) :: _ =>
        val prods = prodSeq.flatMap((nt, alts) => alts.map(nt -> _)).toSet
        CFG(
          prods.map { case (nt, _) => nt }.toSet ++ (for {
            (_, alt) <- prods
            x <- alt.collect { case x: Variable => x }
          } yield x).toSet,
          (for {
            (_, alt) <- prods
            a <- alt.collect { case a: Symbol => a }
          } yield a).toSet,
          start,
          prods,
        )
    }
  private def getId(name: String): Variable =
    if (!name.forall(_.isUpper))
      error(s"the non-upper-case name of nonterminal: $name")
    val base = (for {
      k <- (1 until name.length)
    } yield math.pow(26, k).toInt).sum
    val id = base + name.foldLeft(0) { case (k, c) => k * 26 + (c - 'A') }
    id

  private def ntStr(
    number: Int,
    base: Int = 26,
    conversion: Int => String = x => ('A' + x).toChar.toString,
  ): String =
    def aux(cur: Int, len: Int, mul: Int): String =
      if (cur >= mul) aux(cur - mul, len + 1, mul * base)
      else "'" + Range(0, len).foldLeft(("", cur)) {
        case ((str, cur), _) => (conversion(cur % base) + str, cur / base)
      }.head
    aux(number, 1, base)
}
