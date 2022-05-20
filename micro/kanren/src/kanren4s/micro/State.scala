package kanren4s.micro

final case class State private[kanren4s] (
    substitution: Map[Var, Term],
    variableCounter: Var
) {
  def nextVar: State = copy(variableCounter = variableCounter.next)
}

object State {
  val empty: State = State(Map.empty, Var(0))
}
