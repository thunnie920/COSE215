package kuplrg

object Implementation extends Template {

  // Convert a PDA with final states to a PDA with empty stacks
  //def pdafs2es(pda: PDA): PDA = ???
  def pdafs2es(pda: PDA): PDA = {
    val newStates = pda.states + (-1) + (-2) // -1: q0' || -2: q1'
    val newSymbols = pda.symbols
    val newAlphabets: Set[Alphabet] = pda.alphabets + (-11) // -11 : Z'
    val newinitState = -1 // -1: q0'
    val newinitAlphabet = -11 // -11: Z'
    val newfinalState: Set[State] = Set.empty[State] // final state는 공집합
    val pdaTrans: Map[(State, Option[Symbol], Alphabet), Set[(State, List[Alphabet])]] = pda.trans

    val combinedStackAlphabets = pda.alphabets + (-11)

    val newTransFromq0toq0: Map[(State, Option[Symbol], Alphabet), Set[(State, List[Alphabet])]] =
      Map(((-1, None, -11), Set((pda.initState, List(pda.initAlphabet, -11))))) // 새로 만든 q0'에서 q0로 가는 트랜스 코드 (-1: q0', -11: Z')

    val newTransFromFinalStatestoq1: Map[(State, Option[Symbol], Alphabet), Set[(State, List[Alphabet])]] = {
      val transitions: Set[((State, Option[Symbol], Alphabet), (State, List[Alphabet]))] = for {
        finalState <- pda.finalStates
        stackAlphabet <- combinedStackAlphabets
      } yield {
        ((finalState, None, stackAlphabet), (-2, List.empty[Alphabet]))
      }
      transitions.groupMap(_._1)(_._2).view.mapValues(_.toSet).toMap
    }

    val newTransFromq1toq1: Map[(State, Option[Symbol], Alphabet), Set[(State, List[Alphabet])]] = {
      val transitions: Set[((State, Option[Symbol], Alphabet), (State, List[Alphabet]))] = for {
        stackAlphabet <- combinedStackAlphabets
      } yield {
        ((-2, None, stackAlphabet), (-2, List.empty[Alphabet]))
      }
      transitions.groupMap(_._1)(_._2).view.mapValues(_.toSet).toMap
    }

     val newTrans: Map[(State, Option[Symbol], Alphabet), Set[(State, List[Alphabet])]] =
      (pdaTrans ++ newTransFromq0toq0 ++ newTransFromFinalStatestoq1 ++ newTransFromq1toq1).withDefaultValue(Set())
    
    PDA(newStates, newSymbols, newAlphabets, newTrans, newinitState, newinitAlphabet, newfinalState)

  }


  // Convert a PDA with empty stacks to a PDA with final states
  //def pdaes2fs(pda: PDA): PDA = ???
  def pdaes2fs(pda: PDA): PDA = {
    val newStates = pda.states + (-1) + (-2) // -1: q0' || -2: q1'
    val newSymbols = pda.symbols
    val newAlphabets: Set[Alphabet] = pda.alphabets + (-11) // -11 : Z'
    val newinitState = -1 // -1: q0'
    val newinitAlphabet = -11 // -11: Z'
    val newfinalState: Set[State] = Set(-2) // -2: q1'
    val pdaTrans: Map[(State, Option[Symbol], Alphabet), Set[(State, List[Alphabet])]] = pda.trans //기존 pdafs에서 있었던 트랜스 코드

    val newTransFromq0toq0: Map[(State, Option[Symbol], Alphabet), Set[(State, List[Alphabet])]] =
      Map(((-1, None, -11), Set((pda.initState, List(0, -11)))))


    val newTransFromStatestoq1: Map[(State, Option[Symbol], Alphabet), Set[(State, List[Alphabet])]] = {
      val transitions: Set[((State, Option[Symbol], Alphabet), (State, List[Alphabet]))] = for {
        state <- pda.states
      } yield ((state, None, -11), (-2, List.empty[Alphabet]))
      transitions.groupMap(_._1)(_._2).view.mapValues(_.toSet).toMap
    }

    val newTrans: Map[(State, Option[Symbol], Alphabet), Set[(State, List[Alphabet])]] =
      (pdaTrans ++ newTransFromq0toq0 ++ newTransFromStatestoq1).withDefaultValue(Set())
    
    PDA(newStates, newSymbols, newAlphabets, newTrans, newinitState, newinitAlphabet, newfinalState)

  }


  // Convert a CFG to a PDA with empty stacks
  //def cfg2pdaes(cfg: CFG): PDA = ???
  def cfg2pdaes(cfg: CFG): PDA = {
    val newStates: Set[State] = Set(-1) // -1: q
    val newSymbols: Set[Symbol] = cfg.symbols
    val newAlphabets: Set[Alphabet] = cfg.variables ++ cfg.symbols.map(symbol => symbol: Alphabet)
    val newinitAlphabet: Alphabet = cfg.start
    val newinitState: State = -1 // -1: q
    val newfinalState: Set[State] = Set.empty[State]


    val newTrans1: Map[(State, Option[Symbol], Alphabet), Set[(State, List[Alphabet])]] = {
      val transitions: Set[((State, Option[Symbol], Alphabet), (State, List[Alphabet]))] = for {
        variable <- cfg.variables
        production <- cfg.productions
        if production._1 == variable
      } yield {
        ((-1, None, variable), (-1, production._2.map {
          case v: Variable => v: Alphabet
          case s: Symbol => s
        }))
      }
      transitions.groupMap(_._1)(_._2).view.mapValues(_.toSet).toMap
    }

    val newTrans2: Map[(State, Option[Symbol], Alphabet), Set[(State, List[Alphabet])]] = {
      val transitions: Set[((State, Option[Symbol], Alphabet), (State, List[Alphabet]))] = for {
        symbol <- cfg.symbols
      } yield {
        (((-1, Some(symbol), symbol)), (-1, List.empty[Alphabet]))
      }
      transitions.groupMap(_._1)(_._2).view.mapValues(_.toSet).toMap
    }


    val newTrans: Map[(State, Option[Symbol], Alphabet), Set[(State, List[Alphabet])]] =
      (newTrans1 ++ newTrans2).withDefaultValue(Set())

    PDA(newStates, newSymbols, newAlphabets, newTrans, newinitState, newinitAlphabet, newfinalState)
  }   
  
}

  
