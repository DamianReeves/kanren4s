package kanren4s.kernel

trait GoalRunner {
  // TODO: Perhaps abstract to its own interface
  type Stream = LazyList[Goal]

  def occurs(
      variable: Term.Variable,
      term: Term,
      substitutions: Substitutions
  ): Boolean = {
    val t = walk(term, substitutions)
    t match {
      case Variable(v, _, _) => variable == v
      case _                 => false
    }
  }

  final def run(goal: Goal, state: State): State = state
  def settings: GoalEvaluationSettings
  def unify(lhs: Term, rhs: Term): Substitutions => Option[Substitutions] = s =>
    Some(s)
  def walk(term: Term, substitutions: Substitutions) = {
    def loop(t: Term, result: Option[Term]): Term =
      (t, result) match {
        case (_, Some(resolved)) => resolved
        case (Variable(v, _, _), _) =>
          substitutions.get(v) match {
            case Some(t) => loop(t, None)
            case None    => t
          }
        case _ => t
      }

    loop(term, None)
  }
}

object GoalRunner {
  def default = new GoalRunner {
    override def settings: GoalEvaluationSettings =
      GoalEvaluationSettings.default
  }
}
