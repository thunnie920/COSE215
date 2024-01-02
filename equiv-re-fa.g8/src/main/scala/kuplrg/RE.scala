package kuplrg

import scala.util.parsing.combinator.*
import scala.util.matching.Regex

// The definition of regular expressions
trait RE {

  // A string form of the regular expression.
  lazy val stringForm: String = this match
    case REEmpty() => "</>"
    case REEpsilon() => "<e>"
    case RESymbol(symbol) => s"$symbol"
    case REUnion(left, right) =>
      s"${left.stringForm}|${right.stringForm}"
    case REConcat(left, right) =>
      val l = left match
        case _: REUnion => s"(${left.stringForm})"
        case _ => left.stringForm
      val r = right match
        case _: REUnion => s"(${right.stringForm})"
        case _ => right.stringForm
      s"$l$r"
    case REStar(re) =>
      val s = re match
        case _: REUnion => s"(${re.stringForm})"
        case _ => re.stringForm
      s"$s*"
    case REParen(re) => s"(${re.stringForm})"

  // All symbols in the regular expression
  lazy val symbols: Set[Symbol] = this match
    case REEmpty() => Set()
    case REEpsilon() => Set()
    case RESymbol(symbol) => Set(symbol)
    case REUnion(left, right) => left.symbols ++ right.symbols
    case REConcat(left, right) => left.symbols ++ right.symbols
    case REStar(re) => re.symbols
    case REParen(re) => re.symbols

  // A regex format of the regular expression.
  lazy val regex: Option[Regex] = this match
    case REEmpty() => None
    case REEpsilon() => Some("".r)
    case RESymbol(symbol) => Some(s"$symbol".r)
    case REUnion(left, right) => (left.regex, right.regex) match
      case (Some(l), Some(r)) => Some(s"($l|$r)".r)
      case (Some(l), None) => Some(l)
      case (None, Some(r)) => Some(r)
      case (None, None) => None
    case REConcat(left, right) => for {
      l <- left.regex
      r <- right.regex
    } yield s"($l$r)".r
    case REStar(re) => Some(re.regex.fold("".r) { r => s"($r)*".r })
    case REParen(re) => re.regex

  // The acceptance of a word by the regular expression.
  def accept(word: Word): Boolean = regex.fold(false)(_.matches(word))

  // The language of the regular expression.
  lazy val lang: Lang = (symbols, accept)

  // The simplified regular expression
  lazy val simplified: RE = this match
    case REUnion(l, r) => (l.simplified, r.simplified) match
      case (REEmpty(), r) => r
      case (l, REEmpty()) => l
      case (REEpsilon(), REStar(r)) => REStar(r)
      case (REStar(l), REEpsilon()) => REStar(l)
      case (l, REStar(r)) if l == r => REStar(r)
      case (REStar(l), r) if l == r => REStar(l)
      case (REUnion(REEpsilon(), l), REStar(r)) if l == r => REStar(r)
      case (REUnion(l, REEpsilon()), REStar(r)) if l == r => REStar(r)
      case (REStar(l), REUnion(REEpsilon(), r)) if l == r => REStar(l)
      case (REStar(l), REUnion(r, REEpsilon())) if l == r => REStar(l)
      case (x, REConcat(REStar(y), z)) if x == z => REConcat(REStar(y), z)
      case (REConcat(REStar(x), y), z) if y == z => REConcat(REStar(x), y)
      case (l, r) if l == r => l
      case (l, r) => REUnion(l, r)
    case REConcat(l, r) => (l.simplified, r.simplified) match
      case (REEmpty(), _) => REEmpty()
      case (_, REEmpty()) => REEmpty()
      case (REEpsilon(), r) => r
      case (l, REEpsilon()) => l
      case (REUnion(REEpsilon(), l), REStar(r)) if l == r => REStar(r)
      case (REUnion(l, REEpsilon()), REStar(r)) if l == r => REStar(r)
      case (REStar(l), REUnion(REEpsilon(), r)) if l == r => REStar(l)
      case (REStar(l), REUnion(r, REEpsilon())) if l == r => REStar(l)
      case (l, r) => REConcat(l, r)
    case REStar(re) => re.simplified match
      case REEmpty() => REEpsilon()
      case REEpsilon() => REEpsilon()
      case REUnion(REEpsilon(), r) => REStar(r)
      case REUnion(l, REEpsilon()) => REStar(l)
      case re => REStar(re)
    case REParen(re) => REParen(re.simplified)
    case _ => this

  // Dump the regular expression.
  def dump: Unit =
    show(s"* A regular expression is dumped:")
    println(s"  ${green("* String form:")} ${stringForm}")
    println(s"  ${green("* Scala object:")} ${this}")
}
case class REEmpty() extends RE
case class REEpsilon() extends RE
case class RESymbol(symbol: Symbol) extends RE
case class REUnion(left: RE, right: RE) extends RE
case class REConcat(left: RE, right: RE) extends RE
case class REStar(re: RE) extends RE
case class REParen(re: RE) extends RE

// A parser for regular expressions
object RE extends RegexParsers with PackratParsers {
  def apply(reStr: String): RE = parseAll(union, reStr).get
  lazy val symbol: Parser[Symbol] = "[0-9a-z]".r ^^ { case s => s.head }
  lazy val union: PackratParser[RE] =
    rep1sep(concat, "|") ^^ { _.reduce(REUnion(_, _)) }
  lazy val concat: PackratParser[RE] =
    rep1(star) ^^ { _.reduce(REConcat(_, _)) }
  lazy val star: PackratParser[RE] =
    term ~ rep("*") ^^ { case re ~ s => s.foldLeft(re)((re, _) => REStar(re)) }
  lazy val term: PackratParser[RE] =
    "</>" ^^^ REEmpty() |
    "<e>" ^^^ REEpsilon() |
    symbol ^^ { RESymbol(_) } |
    "(" ~> union <~ ")" ^^ { REParen(_) }
}
