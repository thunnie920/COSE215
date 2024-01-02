package kuplrg

import java.io.{File, PrintWriter}
import io.circe.*, io.circe.syntax.*, io.circe.generic.semiauto.*

// The definition of FA
trait FA:
  val states: Set[State]
  val symbols: Set[Symbol]
  val initState: State
  val finalStates: Set[State]

  // The acceptance of a word
  def accept(w: Word): Boolean

  // The language of the finite automaton
  lazy val lang: Lang = (symbols, accept)

  // A checker for the validity
  lazy val mustValid: Unit = this match
    case dfa: DFA => checkValid(
      symbols,
      dfa.trans,
      !states.contains(_)
    )
    case nfa: NFA => checkValid(
      symbols,
      nfa.trans,
      _.exists(!states.contains(_)),
    )
    case enfa: ENFA => checkValid(
      symbols.map(Some(_)) + None,
      enfa.trans,
      _.exists(!states.contains(_)),
    )
  private def checkValid[Annot, Target](
    annots: Set[Annot],
    trans: Map[(State, Annot), Target],
    invalid: Target => Boolean,
  ): Unit =
    for (q <- states; a <- annots) trans.get((q, a)) match
      case Some(b) => if (invalid(b)) error(s"Invalid transition: ($q, $a) -> $b")
      case None => if (!trans.contains((q, a))) error(s"Missing transition: ($q, $a)")
    if (!states.contains(initState)) error(s"Invalid initial state: $initState")
    val invalidFinals = finalStates.filter(!states.contains(_))
    if (invalidFinals.nonEmpty) error(s"Invalid final states: ${invalidFinals.mkString(", ")}")

  // A checker for counter examples
  def mustEqual(expected: Lang): Unit = mustEqualLang(lang, expected)

  // Dump details of a finite automaton.
  def dump: Unit =
    show("* A DFA is dumped. Please see viewer/index.html")
    val kind = this.getClass.getSimpleName
    val json = Json.obj(
      "kind" -> kind.toLowerCase.asJson,
      "data" -> this.asJson,
      "mapping" -> Json.obj(),
    )
    val file: File = File("viewer/js/data.js")
    val nf = PrintWriter(file)
    nf.print(s"window.data = ${json.noSpacesSortKeys};")
    nf.close()

  // Implicit conversions and encoders
  given Conversion[State, String] = _.toString
  given Conversion[Symbol, String] = _.toString
  given encodeMap[A, B, V](using
    encodeA: Conversion[A, String],
    encodeB: Conversion[B, String],
    encodeV: Encoder[V],
  ): Encoder[Map[(A, B), V]] = new Encoder {
    final def apply(map: Map[(A, B), V]): Json =
      val pairs = map.map {
        case ((a, b), v) => (a.toString, b.toString) -> v
      }.toList.sortBy(_._1)
      Json.fromFields(pairs.groupBy(_._1._1).map {
        case (a, pairs) => a -> Json.fromFields(pairs.map {
          case ((_, b), v) => b -> encodeV(v)
        })
      })
  }
  given encodeOptMap[A, B, V](using
    encodeA: Conversion[A, String],
    encodeB: Conversion[B, String],
    encodeV: Encoder[V],
  ): Encoder[Map[(A, Option[B]), V]] = new Encoder {
    final def apply(map: Map[(A, Option[B]), V]): Json =
      val pairs = map.map {
        case ((a, b), v) =>
          val bStr = b.map(_.toString).getOrElse("")
          (a.toString, bStr) -> v
      }.toList.sortBy(_._1)
      Json.fromFields(pairs.groupBy(_._1._1).map {
        case (a, pairs) => a -> Json.fromFields(pairs.map {
          case ((_, b), v) => b -> encodeV(v)
        })
      })
  }
  given Encoder[DFA] = deriveEncoder[DFA]
  given Encoder[NFA] = deriveEncoder[NFA]
  given Encoder[ENFA] = deriveEncoder[ENFA]
  given Encoder[FA] = Encoder.instance {
    case dfa: DFA => dfa.asJson
    case nfa: NFA => nfa.asJson
    case enfa: ENFA => enfa.asJson
  }
