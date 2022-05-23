package kanren4s.core

trait GoalModule { self: StateModule =>
  sealed trait Goal extends Product with Serializable { self =>
    import Goal._

    def ||(that: Goal): Goal = Or(this, that)
    def or(that: Goal): Goal = Or(self, that)

    def apply(state: State): StateStream = {
      self match {
        case Eq(t1, t2) =>
          unify(t1, t2, state.substitution) match {
            case None => StateStream.empty
            case Some(value) =>
              StateStream.single(state.withSubstitution(value))
          }
        case Or(g1, g2) =>
          val first = g1(state)
          val second = g2(state)
          StateStream.append(first, second)
        case And(g1, g2) =>
          val states = g1(state)
          StateStream.bind(states, g2)
        case Fail =>
          StateStream.empty
        case Fresh(block) =>
          val (v, newState) = state.freshVariable()
          val goal = block(v)
          goal(newState)
        case FromFunction(f) => f(state)
        case Succceed        => StateStream.single(state)
      }
    }
  }
  object Goal {
    def eq(x: Term, y: Term): Goal = Eq(x, y)
    def fresh(block: Var => Goal): Goal = Fresh(block)
    def fresh(block: (Var, Var) => Goal): Goal =
      Fresh((x: Var) => Fresh((y: Var) => block(x, y)))
    def or(left: Goal, right: Goal): Goal = Or(left, right)
    def and(left: Goal, right: Goal): Goal = And(left, right)

    private case class And(g1: Goal, g2: Goal) extends Goal
    private case class Eq(a: Term, b: Term) extends Goal
    private case object Fail extends Goal
    private case class FromFunction(f: State => StateStream) extends Goal
    private case class Fresh(block: Var => Goal) extends Goal
    private case class Or(g1: Goal, g2: Goal) extends Goal
    private case object Succceed extends Goal
  }

}