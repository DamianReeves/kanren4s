package kanren4s.kernel

object GoalEvaluationSettings {
  val default = GoalEvaluationSettings(enableOccursCheck = true)
}

final case class GoalEvaluationSettings(enableOccursCheck: Boolean)