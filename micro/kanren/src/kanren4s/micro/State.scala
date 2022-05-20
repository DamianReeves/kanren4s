package kanren4s.micro

final case class State private[kanren4s] (
    substitution: Map[Var, Term],
    variableCounter: Var
)

object State {
  def empty: State = State(Map.empty, Var(0))
}
