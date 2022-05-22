package kanren4s.kernel

sealed trait Goal2 extends Product with Serializable { self =>
  import Goal2._
  import ResultsCollector.DefaultStreamType

  def ||(that: Goal2): Goal2 = Or(this, that)
  def or(that: Goal2): Goal2 = Or(self, that)

  def apply(state: State): DefaultStreamType =
    apply(state, GoalEvalSettings.default)

  def apply[ResultsStream, Collector <: ResultsCollector.OfType[ResultsStream]](
      state: State,
      settings: GoalEvalSettings[ResultsStream, Collector]
  ): ResultsStream = {
    val results: Collector = settings.results
    self match {
      case Eq(t1, t2) =>
        state.substitutions.unify(t1, t2) match {
          case None        => results.mzero
          case Some(value) => results.mkStream(state.withSubstitutions(value))
        }
      case Or(g1, g2) =>
        val first = g1(state, settings)
        val second = g2(state, settings)
        results.append(first, second)
      case And(g1, g2) =>
        val states = g1(state, settings)
        results.bind(states, g2)(settings)
      case Fresh(block) =>
        val (v, newState) = state.freshVariable()
        println(s"Have a fresh var: $v")
        val goal = block(v)
        goal.apply(newState, settings)
    }
  }
}
object Goal2 {
  def eq(x: Term, y: Term): Goal2 = Eq(x, y)
  def fresh(block: Variable => Goal2): Goal2 = Fresh(block)
  def fresh(block: (Variable, Variable) => Goal2): Goal2 =
    Fresh((x: Variable) => Fresh((y: Variable) => block(x, y)))
  def or(left: Goal2, right: Goal2): Goal2 = Or(left, right)

  final case class Settings()
  trait EvaluationContext {
    def sett
  }

  private[kernel] final case class And(g1: Goal2, g2: Goal2) extends Goal2
  private[kernel] final case class Eq(a: Term, b: Term) extends Goal2
  private[kernel] final case class Fresh(block: Variable => Goal2) extends Goal2
  private[kernel] final case class Or(g1: Goal2, g2: Goal2) extends Goal2
  // private[kernel] final case class Succceed() extends Goal2
}
