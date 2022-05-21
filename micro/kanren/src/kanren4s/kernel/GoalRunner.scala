package kanren4s.kernel

trait GoalRunner {
  // TODO: Perhaps abstract to its own interface
  type Stream = LazyList[Goal]

  final def run(goal: Goal, state: State): State = state
  def settings: GoalEvaluationSettings

  def unify(lhs: Term, rhs: Term, s: Substitutions): Option[Substitutions] = {
    val t1 = s.walk(lhs)
    val t2 = s.walk(rhs)

    if (t1 == t2)
      // the terms are equal, no unification is needed
      Some(s)
    else
      (t1, t2) match {
        case (Variable(v, _, _), t2) =>
          // Try and extend the substitution since we have a variable on the left
          s.extend(v, t2)
        case (t1, Variable(v, _, _)) =>
          // Try and unify by flipping since we have a variable on the right
          unify(v, t1, s)
        case (Term.Pair(l1, r1), Term.Pair(l2, r2)) =>
          // Unify the left and right sides of the pair
          unify(l1, l2, s).flatMap(s1 => unify(r1, r2, s1))
        case _ =>
          // The terms are not equal and cannot be unified
          None
      }
  }

}

object GoalRunner {
  def default = new GoalRunner {
    override def settings: GoalEvaluationSettings =
      GoalEvaluationSettings.default
  }
}
