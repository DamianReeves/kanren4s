package kanren4s.kernel

sealed trait Goal extends Product with Serializable { self =>
  import Goal._

  final def run(): State = run(State.empty)

  final def run(state: State): State = runWith(state)(GoalRunner.default)

  final def runWith(runner: GoalRunner): State =
    runner.run(self, State.empty)
  final def runWith(state: State)(runner: GoalRunner): State =
    runner.run(self, state)
}
object Goal {
  def eq(x: Term, y: Term): Goal = Eq(x, y)
  def fresh(block: Term.Variable => Goal): Goal = Fresh(block)

  final case class Settings()
  trait EvaluationContext {
    def sett
  }

  private final case class Eq(a: Term, b: Term) extends Goal
  private final case class Disj(g1: Goal, g2: Goal) extends Goal
  private final case class Conj(g1: Goal, g2: Goal) extends Goal
  private final case class Fresh(get: Term.Variable => Goal) extends Goal
}
