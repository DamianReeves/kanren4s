package kanren4s.kernel

final case class State(
    substitutions: Substitution,
    nextVariableId: VariableId
) { self =>

  def freshVariable(): (Var, State) = {
    val id = nextVariableId
    val next = VariableId.next(id)
    (Var.anonymous(id), withNextVariableId(next))
  }

  def freshVariable(label: String): (Var, State) = {
    val id = nextVariableId
    val next = VariableId.next(id)
    (Var.labeled(label, id), withNextVariableId(next))
  }

  def withNextIndex(nextIdx: Int): State =
    copy(nextVariableId = VariableId.from(nextIdx))
  def withNextVariableId(nextVariableId: VariableId): State =
    copy(nextVariableId = nextVariableId)
  def withSubstitutions(substitutions: Substitution): State =
    copy(substitutions = substitutions)
  def withSubstitutions(bindings: (Var, Term)*): State =
    copy(substitutions = Substitution(bindings.toMap))
}
object State {
  val empty: State = State(Substitution.empty, VariableId.first)
}
