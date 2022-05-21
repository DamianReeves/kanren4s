package kanren4s.kernel

final case class Substitutions(bindings: Map[Variable, Term]) { self =>
  def get(variable: Variable): Option[Term] = bindings.get(variable)
}
object Substitutions {
  val empty: Substitutions = Substitutions(Map.empty)
}
