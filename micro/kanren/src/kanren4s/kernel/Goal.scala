package kanren4s.kernel

sealed trait Goal extends Product with Serializable { self =>
  import Goal._

  final def run(): GoalRunner.default.Stream = run(State.empty)

  final def run(state: State): GoalRunner.default.Stream =
    runWith(state)(GoalRunner.default)

  final def runWith(runner: GoalRunner): runner.Stream =
    runner.run(self, State.empty)
  final def runWith(state: State)(runner: GoalRunner): runner.Stream =
    runner.run(self, state)
}
object Goal {
  def eq(x: Term, y: Term): Goal = Eq(x, y)
  def fresh(block: Term.Variable => Goal): Goal = Fresh(block)

  final case class Settings()
  trait EvaluationContext {
    def sett
  }

  private[kernel] final case class Eq(a: Term, b: Term) extends Goal
  private[kernel] final case class Disj(g1: Goal, g2: Goal) extends Goal
  private[kernel] final case class Conj(g1: Goal, g2: Goal) extends Goal
  private[kernel] final case class Fresh(get: Term.Variable => Goal)
      extends Goal
}
