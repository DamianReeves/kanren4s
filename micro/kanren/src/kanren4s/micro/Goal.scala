package kanren4s.micro
import kanren4s.micro.Goal.Disj
import kanren4s.micro.Goal.Conj
import kanren4s.micro.Goal.Fresh
import kanren4s.micro.Goal.Delay

sealed trait Goal extends Product with Serializable { self =>
  def toString(variableCount: Int): String = self match {
    case Goal.Equiv(a, b) => s"($a ≡ $b)"
    case Disj(g1, g2)     => s"(($g1) OR ($g2))"
    case Conj(g1, g2)     => s"(($g1) AND ($g2))"
    case Fresh(get) =>
      if (variableCount > 4)
        "<exists x: ...>"
      else
        s"exists x$variableCount: (${get(Var(variableCount)).toString(variableCount + 1)})"
    case Delay(g) => s"delayed (${g().toString(variableCount)})"
  }

  override def toString(): String = self.toString(0)
}
object Goal {
  val always: Goal = equiv(
    Term.Symbol("_always", List.empty),
    Term.Symbol("_always", List.empty)
  )
  val never: Goal =
    equiv(Term.Symbol("_never", List.empty), Term.Symbol("_never2", List.empty))

  def callFresh(f: Var => Goal): Goal = Fresh(f)
  def delay(g: => Goal): Goal = Delay(() => g)
  def disj(goal1: Goal, goal2: Goal): Goal = Disj(goal1, goal2)
  def conj(goal1: Goal, goal2: Goal): Goal = Conj(goal1, goal2)
  def equiv(term1: Term, term2: Term): Goal = Equiv(term1, term2)

  private final case class Equiv(a: Term, b: Term) extends Goal
  private final case class Disj(g1: Goal, g2: Goal) extends Goal
  private final case class Conj(g1: Goal, g2: Goal) extends Goal
  private final case class Fresh(get: Var => Goal) extends Goal
  private final case class Delay(g: () => Goal) extends Goal
}
