package kanren4s.core

trait GoalModule extends StateModule { self =>

  sealed trait Goal extends Product with Serializable { self =>
    import Goal._

    def ||(that: Goal): Goal = Goal.or(this, that)
    def or(that: Goal): Goal = Goal.or(self, that)

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
          StateStream.bind(states, g2(_))
        case Fail =>
          StateStream.empty
        case FromFunction(f) => f(state)
        case Snooze(goal) =>
          StateStream.suspend(() => goal(state))
        case Succceed => StateStream.single(state)
      }
    }
  }
  object Goal {

    val fail: Goal = Fail
    val succeed: Goal = Succceed

    def and(left: Goal, right: Goal): Goal = And(left, right)
    def callFresh(f: Var => Goal): Goal = Goal.fromFunction {
      case State(subst, seqNum) =>
        f(Var(seqNum))(State(subst, seqNum.next))
    }

    def eq(x: Term, y: Term): Goal = Eq(x, y)
    def fromFunction(f: State => StateStream): Goal = Snooze(FromFunction(f))
    def or(left: Goal, right: Goal): Goal = Or(left, right)

    def snooze(goal: Goal): Goal = Snooze(goal)

    private case class And(g1: Goal, g2: Goal) extends Goal
    private case class Eq(a: Term, b: Term) extends Goal
    private case object Fail extends Goal
    private case class FromFunction(f: State => StateStream) extends Goal
    private case class Or(g1: Goal, g2: Goal) extends Goal
    private case class Snooze(g: Goal) extends Goal
    private case object Succceed extends Goal
  }

}
