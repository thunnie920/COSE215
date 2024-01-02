package kuplrg

trait Template {

  // A transition allowing epsilon-transition
  type Transition = (State, Option[Symbol], State)

  // A simplified epsilon-NFA
  case class SimpleENFA(from: State, trans: Set[Transition], to: State)

  // Convert a regular expression to a epsilon-NFA
  def re2enfa(re: RE): ENFA

  // Convert a DFA to a regular expression
  def dfa2re(givenDFA: DFA, debug: Boolean = false): RE
}
