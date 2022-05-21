package kanren4s.kernel

final case class State(substitutions: Substitutions, nextVar: Variable) {
  self =>
}
object State {
  val empty: State = State(Substitutions.empty, Variable.first)
}
