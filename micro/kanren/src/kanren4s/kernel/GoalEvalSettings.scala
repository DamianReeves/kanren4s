package kanren4s.kernel

object GoalEvalSettings {
  val default: GoalEvalSettings[ResultsCollector.DefaultStreamType, ResultsCollector.Default] =
    GoalEvalSettings(enableOccursCheck = true, ResultsCollector.Default)
}

final case class GoalEvalSettings[ResultsStream,Collector <: ResultsCollector.OfType[ResultsStream]](
    enableOccursCheck: Boolean,
    results: Collector
) { self => }
