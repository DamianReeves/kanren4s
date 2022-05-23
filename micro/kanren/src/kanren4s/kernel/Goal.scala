package kanren4s.kernel

sealed trait Goal extends Product with Serializable { self =>
  import Goal._

  def ||(that: Goal): Goal = Or(this, that)
  def or(that: Goal): Goal = Or(self, that)

  def apply(state: State): ResultStream = {
    self match {
      case Eq(t1, t2) =>
        state.substitutions.unify(t1, t2) match {
          case None => ResultStream.empty
          case Some(value) =>
            ResultStream.single(state.withSubstitutions(value))
        }
      case Or(g1, g2) =>
        val first = g1(state)
        val second = g2(state)
        ResultStream.append(first, second)
      case And(g1, g2) =>
        val states = g1(state)
        ResultStream.bind(states, g2)
      case Fresh(block) =>
        val (v, newState) = state.freshVariable()
        val goal = block(v)
        goal(newState)
    }
  }
}
object Goal {
  def eq(x: Term, y: Term): Goal = Eq(x, y)
  def fresh(block: Var => Goal): Goal = Fresh(block)
  def fresh(block: (Var, Var) => Goal): Goal =
    Fresh((x: Var) => Fresh((y: Var) => block(x, y)))
  def or(left: Goal, right: Goal): Goal = Or(left, right)

  final case class Settings()
  trait EvaluationContext {
    def sett
  }

  private[kernel] final case class And(g1: Goal, g2: Goal) extends Goal
  private[kernel] final case class Eq(a: Term, b: Term) extends Goal
  private[kernel] final case class Fresh(block: Var => Goal) extends Goal
  private[kernel] final case class Or(g1: Goal, g2: Goal) extends Goal
  // private[kernel] final case class Succceed() extends Goal
}
