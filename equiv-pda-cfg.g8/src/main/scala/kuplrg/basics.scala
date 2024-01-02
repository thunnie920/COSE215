package kuplrg

import scala.Console.*
import scala.collection.mutable

// The type definitions of states
type State = Int

// The type definitions of symbols
type Symbol = Char

// The type definitions of alphabets
type Alphabet = Int

// The type definitions of variables
type Variable = Int

// The type definition of words
type Word = String

// The type definition of configurations
type Config = (State, Word, List[Alphabet])

// The type definition of languages
type Lang = (Set[Symbol], Word => Boolean)

// A helper function to extract first symbol and rest of word
object `<|` { def unapply(w: Word) = w.headOption.map((_, w.drop(1))) }

// Memoize a function for better performance
def memoize[I, O](f: I => O): I => O = new mutable.HashMap[I, O]() {
  override def apply(key: I) = getOrElseUpdate(key, f(key))
}

// A helper function to print a message in green
def show(msg: String): Unit = println(green(msg))
def green(str: String): String = s"$GREEN$str$RESET"

// The equality check of two languages
val TRIAL: Int = 1000
def mustEqualLang(target: Lang, expected: Lang, msg: String = ""): Unit =
  val (_, accept) = target
  val (symbols, expectedAccept) = expected
  val list = symbols.toList.sorted
  val m = symbols.size
  def check(s: Word): Unit =
    val result = accept(s)
    val answer = expectedAccept(s)
    if (result != answer)
      val neg = if (answer) "" else " not"
      val errMsg =
        if (msg == "") s"the word '$s' should$neg be in the language."
        else msg
      error(errMsg)
  def aux(n: Int, k: Int, prevSize: Int): Unit =
    val curSize = (prevSize * m) min n
    for {
      i <- 0 until curSize
      (s, _) = (0 until k).foldLeft(("", i)) {
        case ((s, j), _) => (list(j % m).toString + s, j / list.length)
      }
    } check(s)
    if (curSize < n) aux(n - curSize, k + 1, curSize)
  m match
    case 0 => check("")
    case 1 =>
      val s = symbols.head.toString
      (0 until TRIAL / 10).map(i => check(s * i))
    case _ => aux(TRIAL, 1, 1)
