package kuplrg

import Implementation.*

class Spec extends SpecBase {

  test(dumpTarget.dump)

  // tests for `dfa_w00`
  val lang_w00: Lang = (
    "01".toSet,
    _.endsWith("00")
  )
  test(dfa_w00.mustValid)
  test(dfa_w00.mustEqual(lang_w00))

  // tests for `dfa_a_star_b`
  val lang_a_star_b: Lang = (
    "ab".toSet,
    "a*b".r.matches
  )
  test(dfa_a_star_b.mustValid)
  test(dfa_a_star_b.mustEqual(lang_a_star_b))

  // tests for `dfa_div_3`
  val lang_div_3: Lang = (
    "01".toSet,
    w => Integer.parseInt(w, 2) % 3 == 0
  )
  test(dfa_div_3.mustValid)
  test(dfa_div_3.mustEqual(lang_div_3))

  // tests for `dfa_subseq_011`
  val lang_subseq_011: Lang = (
    "01".toSet,
    ".*0.*1.*1.*".r.matches
  )
  test(dfa_subseq_011.mustValid)
  test(dfa_subseq_011.mustEqual(lang_subseq_011))

  // tests for `dfa_even_0_1`
  val lang_even_0_1: Lang = (
    "01".toSet,
    w => w.count(_ == '0') % 2 == 0 && w.count(_ == '1') % 2 == 0,
  )
  test(dfa_even_0_1.mustValid)
  test(dfa_even_0_1.mustEqual(lang_even_0_1))

  // tests for `nfa_least_two_0`
  val lang_least_two_0: Lang = (
    "01".toSet,
    _.count(_ == '0') >= 2
  )
  test(nfa_least_two_0.mustValid)
  test(nfa_least_two_0.mustEqual(lang_least_two_0))

  // tests for `nfa_two_0`
  val lang_two_0: Lang = (
    "01".toSet,
    _.count(_ == '0') == 2
  )
  test(nfa_two_0.mustValid)
  test(nfa_two_0.mustEqual(lang_two_0))

  // tests for `nfa_substr_000`
  val lang_substr_000: Lang = (
    "01".toSet,
    _.contains("000")
  )
  test(nfa_substr_000.mustValid)
  test(nfa_substr_000.mustEqual(lang_substr_000))

  // tests for `enfa_ab_plus`
  val lang_ab_plus: Lang = (
    "ab".toSet,
    "(ab)+".r.matches
  )
  test(enfa_ab_plus.mustValid)
  test(enfa_ab_plus.mustEqual(lang_ab_plus))

  // tests for `enfa_same_digits`
  val lang_same_digits: Lang = (
    "01".toSet,
    w => w.forall(_ == '0') || w.forall(_ == '1')
  )
  test(enfa_same_digits.mustValid)
  test(enfa_same_digits.mustEqual(lang_same_digits))

  // tests for `enfa_aibjck`
  val lang_aibjck: Lang = ("abc".toSet, "a*b*c*".r.matches)
  test(enfa_aibjck.mustValid)
  test(enfa_aibjck.mustEqual(lang_aibjck))

  /* Write your own tests */
}
