package kanren4s.micro

final case class State private[kanren4s] (
    substitution: Map[Variable, Term],
    variableCounter: Variable
)

object State {
  def empty: State = State(Map.empty, Variable(0))
}
