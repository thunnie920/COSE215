package kuplrg

import Implementation.*

class Spec extends SpecBase {

  // The playground for tests
  def afterTest: Unit = {

    val dfa: DFA = DFA(
      states = Set(0, 1, 2),
      symbols = Set('0', '1'),
      trans = Map(
        (0, '0') -> 1,
        (0, '1') -> 0,
        (1, '0') -> 2,
        (1, '1') -> 0,
        (2, '0') -> 2,
        (2, '1') -> 0,
      ),
      initState = 0,
      finalStates = Set(2),
    )


    // You can dump any finite automaton via `dump` method
    dfa.dump

    // You can see the string form and Scala object of the regular expression
    re1.dump

    // You can see the detailed process of `dfa2re`,
    // please invoke it with `debug = true` option as follows:
    dfa2re(dfa, debug = true)
  }

  // Tests for `re2enfa`
  val re1: RE = RE("</>")
  test(mustEqualLang(re2enfa(re1).lang, re1.lang))
  val re2: RE = RE("<e>")
  test(mustEqualLang(re2enfa(re2).lang, re2.lang))
  val re3: RE = RE("a")
  test(mustEqualLang(re2enfa(re3).lang, re3.lang))
  val re4: RE = RE("a|b")
  test(mustEqualLang(re2enfa(re4).lang, re4.lang))
  val re5: RE = RE("ab")
  test(mustEqualLang(re2enfa(re5).lang, re5.lang))
  val re6: RE = RE("</>a")
  test(mustEqualLang(re2enfa(re6).lang, re6.lang))
  val re7: RE = RE("0*")
  test(mustEqualLang(re2enfa(re7).lang, re7.lang))
  val re8: RE = RE("10*")
  test(mustEqualLang(re2enfa(re8).lang, re8.lang))
  val re9: RE = RE("(a|b)*")
  test(mustEqualLang(re2enfa(re9).lang, re9.lang))
  val re10: RE = RE("((a))")
  test(mustEqualLang(re2enfa(re10).lang, re10.lang))
  val re11: RE = RE("a|a|a")
  test(mustEqualLang(re2enfa(re11).lang, re11.lang))
  val re12: RE = RE("0101010101")
  test(mustEqualLang(re2enfa(re12).lang, re12.lang))
  val re13: RE = RE("a*b*")
  test(mustEqualLang(re2enfa(re13).lang, re13.lang))
  val re14: RE = RE("1*01*01*")
  test(mustEqualLang(re2enfa(re14).lang, re14.lang))
  val re15: RE = RE("(0|1)*000(0|1)*")
  test(mustEqualLang(re2enfa(re15).lang, re15.lang))
  val re16: RE = RE("(a|<e>)(ba)*|(b|<e>)(ab)*")
  test(mustEqualLang(re2enfa(re16).lang, re16.lang))
  val re17: RE = RE("aaaa*(bb)*")
  test(mustEqualLang(re2enfa(re17).lang, re17.lang))
  val re18: RE = RE("(aa)*(ab|<e>)(bb)*")
  test(mustEqualLang(re2enfa(re18).lang, re18.lang))
  val re19: RE = RE("1*(01*01*01*)*")
  test(mustEqualLang(re2enfa(re19).lang, re19.lang))
  val re20: RE = RE("(0|1(01*0)*1)*")
  test(mustEqualLang(re2enfa(re20).lang, re20.lang))

  // Tests for `dfa2re`
  val dfa1: DFA = DFA(1, "01", 0, 0)
  test(mustEqualLang(dfa2re(dfa1).lang, dfa1.lang))
  val dfa2: DFA = DFA(2, "01", 3, 2)
  test(mustEqualLang(dfa2re(dfa2).lang, dfa2.lang))
  val dfa3: DFA = DFA(2, "ab", 5, 2)
  test(mustEqualLang(dfa2re(dfa3).lang, dfa3.lang))
  val dfa4: DFA = DFA(2, "xy", 13, 2)
  test(mustEqualLang(dfa2re(dfa4).lang, dfa4.lang))
  val dfa5: DFA = DFA(3, "01", 181, 4)
  test(mustEqualLang(dfa2re(dfa5).lang, dfa5.lang))
  val dfa6: DFA = DFA(3, "ab", 723, 2)
  test(mustEqualLang(dfa2re(dfa6).lang, dfa6.lang))
  val dfa7: DFA = DFA(3, "01", 588, 8)
  test(mustEqualLang(dfa2re(dfa7).lang, dfa7.lang))
  val dfa8: DFA = DFA(3, "ab", 205, 4)
  test(mustEqualLang(dfa2re(dfa8).lang, dfa8.lang))
  val dfa9: DFA = DFA(3, "01", 43, 4)
  test(mustEqualLang(dfa2re(dfa9).lang, dfa9.lang))
  val dfa10: DFA = DFA(3, "abcd", 203492, 1)
  test(mustEqualLang(dfa2re(dfa10).lang, dfa10.lang))
  val dfa11: DFA = DFA(3, "xyz", 12984, 1)
  test(mustEqualLang(dfa2re(dfa11).lang, dfa11.lang))
  val dfa12: DFA = DFA(3, "ab", 456, 3)
  test(mustEqualLang(dfa2re(dfa12).lang, dfa12.lang))
  val dfa13: DFA = DFA(4, "01", 65169, 8)
  test(mustEqualLang(dfa2re(dfa13).lang, dfa13.lang))
  val dfa14: DFA = DFA(4, "01", 25545, 1)
  test(mustEqualLang(dfa2re(dfa14).lang, dfa14.lang))
  val dfa15: DFA = DFA(4, "ab", 52342, 4)
  test(mustEqualLang(dfa2re(dfa15).lang, dfa15.lang))
  val dfa16: DFA = DFA(4, "ab", 18242, 6)
  test(mustEqualLang(dfa2re(dfa16).lang, dfa16.lang))
  val dfa17: DFA = DFA(5, "01", 2849302, 12)
  test(mustEqualLang(dfa2re(dfa17).lang, dfa17.lang))
  val dfa18: DFA = DFA(5, "ab", 10901436, 23)
  test(mustEqualLang(dfa2re(dfa18).lang, dfa18.lang))
  val dfa19: DFA = DFA(6, "01", 38429324, 9)
  test(mustEqualLang(dfa2re(dfa19).lang, dfa19.lang))
  val dfa20: DFA = DFA(7, "01", 1092334322, 25)
  test(mustEqualLang(dfa2re(dfa20).lang, dfa20.lang))

  /* Write your own tests */
}
