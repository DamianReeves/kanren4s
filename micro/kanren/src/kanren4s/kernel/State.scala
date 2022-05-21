package kanren4s.kernel

final case class State(
    substitutions: Substitutions,
    nextVariableId: VariableId
) { self =>
  def withNextIndex(nextIdx: Int): State =
    copy(nextVariableId = VariableId.from(nextIdx))
  def withNextVariableId(nextVariableId: VariableId): State =
    copy(nextVariableId = nextVariableId)
  def withSubstitutions(substitutions: Substitutions): State =
    copy(substitutions = substitutions)
}
object State {
  val empty: State = State(Substitutions.empty, VariableId.first)
}
