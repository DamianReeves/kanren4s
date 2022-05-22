package kanren4s.kernel

sealed trait Goal extends Product with Serializable { self =>
  import Goal._
  import ResultsCollector.DefaultStreamType

  def ||(that: Goal): Goal = Or(this, that)
  def or(that: Goal): Goal = Or(self, that)

  def apply(state: State): DefaultStreamType =
    apply(state, GoalEvalSettings.default)

  def apply[ResultsStream, Collector <: ResultsCollector.OfType[ResultsStream]](
      state: State,
      settings: GoalEvalSettings[ResultsStream, Collector]
  ): ResultsStream = {
    val results: Collector = settings.results
    self match {
      case Eq(a, b)    => evalEq(a, b, state, settings)
      case Or(g1, g2)  => evalOr(g1, g2, state, settings)
      case And(g1, g2) => results.single(state)
      case Fresh(block) =>
        val (v,newState) = state.freshVariable()
        println(s"Have a fresh var: $v")
        val goal = block(v)
        goal.apply(newState, settings)
    }
  }

  private def evalEq[RS, Col <: ResultsCollector.OfType[RS]](
      t1: Term,
      t2: Term,
      state: State,
      settings: GoalEvalSettings[RS, Col]
  ): RS = {
    val results = settings.results
    state.substitutions.unify(t1, t2) match {
      case None        => results.mzero
      case Some(value) => results.mkStream(state.withSubstitutions(value))
    }
  }

  private def evalOr[RS, Col <: ResultsCollector.OfType[RS]](
      g1: Goal,
      g2: Goal,
      state: State,
      settings: GoalEvalSettings[RS, Col]
  ): RS = {
    val results = settings.results
    val first = g1(state, settings)
    val second = g2(state, settings)
    results.append(first, second)
  }
}
object Goal {
  def eq(x: Term, y: Term): Goal = Eq(x, y)
  def fresh(block: Variable => Goal): Goal = Fresh(block)
  def fresh(block: (Variable, Variable) => Goal): Goal =
    Fresh((x: Variable) => Fresh((y: Variable) => block(x, y)))
  def or(left: Goal, right: Goal): Goal = Or(left, right)

  final case class Settings()
  trait EvaluationContext {
    def sett
  }

  private[kernel] final case class Eq(a: Term, b: Term) extends Goal
  private[kernel] final case class Or(g1: Goal, g2: Goal) extends Goal
  private[kernel] final case class And(g1: Goal, g2: Goal) extends Goal
  private[kernel] final case class Fresh(block: Variable => Goal) extends Goal
}
