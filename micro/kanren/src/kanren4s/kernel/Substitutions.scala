package kanren4s.kernel

final case class Substitutions(bindings: Map[Term.Variable, Term]) { self =>
  def get(variable: Term.Variable): Option[Term] = bindings.get(variable)
}
object Substitutions {
  val empty: Substitutions = Substitutions(Map.empty)
}
