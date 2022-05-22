package kanren4s.kernel

final case class State(
    substitutions: Substitution,
    nextVariableId: VariableId
) { self =>

  def freshVariable(): (Variable, State) = {
    val id = nextVariableId
    val next = VariableId.next(id)
    (Variable.anonymous(id), withNextVariableId(next))
  }

  def freshVariable(label: String): (Variable, State) = {
    val id = nextVariableId
    val next = VariableId.next(id)
    (Variable.labeled(label, id), withNextVariableId(next))
  }

  def withNextIndex(nextIdx: Int): State =
    copy(nextVariableId = VariableId.from(nextIdx))
  def withNextVariableId(nextVariableId: VariableId): State =
    copy(nextVariableId = nextVariableId)
  def withSubstitutions(substitutions: Substitution): State =
    copy(substitutions = substitutions)
  def withSubstitutions(bindings: (Variable, Term)*): State =
    copy(substitutions = Substitution(bindings.toMap))
}
object State {
  val empty: State = State(Substitution.empty, VariableId.first)
}
