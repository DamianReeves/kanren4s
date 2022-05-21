package kanren4s.kernel

trait GoalRunner {
  // TODO: Perhaps abstract to its own interface
  type Stream = LazyList[Goal]

  final def run(goal: Goal, state: State): State = state
  def settings: GoalEvaluationSettings


}

object GoalRunner {
  def default = new GoalRunner {
    override def settings: GoalEvaluationSettings =
      GoalEvaluationSettings.default
  }
}
