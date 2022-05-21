package kanren4s.kernel

final case class Substitutions(bindings: Map[Variable, Term]) { self =>

  def extend(variable: Variable, term: Term): Option[Substitutions] = {
    if (occurs(variable, term)) None
    else Some(Substitutions(bindings + (variable -> term)))
  }

  def occurs(
      variable: Variable,
      term: Term
  ): Boolean = {
    val t = walk(term)
    t match {
      case Variable(v, _, _) => variable == v
      case Term.Pair(left, right) =>
        occurs(variable, left) || occurs(variable, right)
      case _ => false
    }
  }

  def walk(term: Term): Term = {
    def loop(t: Term, result: Option[Term]): Term =
      (t, result) match {
        case (_, Some(resolved)) => resolved
        case (Variable(v, _, _), _) =>
          bindings.get(v) match {
            case Some(t) => loop(t, None)
            case None    => t
          }
        case _ => t
      }

    loop(term, None)
  }
}
object Substitutions {
  val empty: Substitutions = Substitutions(Map.empty)
}
