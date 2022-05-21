package kanren4s.kernel

sealed trait Goal extends Product with Serializable { self =>
  import Goal._
  import ResultsCollector.DefaultStreamType

  def ||(that: Goal): Goal = Or(this, that)
  def or(that: Goal): Goal = Or(self, that)

  def apply(state: State): DefaultStreamType =
    apply(state, GoalEvalSettings.default)

  def apply[ResultsStream,Collector <: ResultsCollector.OfType[ResultsStream]](
      state: State,
      settings: GoalEvalSettings[ResultsStream, Collector]
  ): ResultsStream = {
    val results: Collector = settings.results
    self match {
      case Eq(a, b)     => evalEq(a, b, state, settings)
      case Or(g1, g2)   => results.single(state)
      case Conj(g1, g2) => results.single(state)
      case Fresh(get)   => results.single(state)
    }
  }

  private def evalEq[ResultsStream,Collector <: ResultsCollector.OfType[ResultsStream]](
      t1: Term,
      t2: Term,
      state: State,
      settings: GoalEvalSettings[ResultsStream, Collector]
  ): ResultsStream = {
    val results = settings.results
    state.substitutions.unify(t1, t2) match {
      case None        => results.mzero
      case Some(value) => results.mkStream(state.withSubstitutions(value))
    }
  }
}
object Goal {
  def eq(x: Term, y: Term): Goal = Eq(x, y)
  def fresh(block: Term.Variable => Goal): Goal = Fresh(block)
  def or(left: Goal, right: Goal): Goal = Or(left, right)

  final case class Settings()
  trait EvaluationContext {
    def sett
  }

  private[kernel] final case class Eq(a: Term, b: Term) extends Goal
  private[kernel] final case class Or(g1: Goal, g2: Goal) extends Goal
  private[kernel] final case class Conj(g1: Goal, g2: Goal) extends Goal
  private[kernel] final case class Fresh(get: Term.Variable => Goal)
      extends Goal
}
