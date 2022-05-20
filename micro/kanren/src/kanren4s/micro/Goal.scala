package kanren4s.micro
import kanren4s.micro.Goal.Disj
import kanren4s.micro.Goal.Conj
import kanren4s.micro.Goal.Fresh
import kanren4s.micro.Goal.Delay
import kanren4s.micro.Term.Variable

sealed trait Goal extends Product with Serializable { self =>
  import Goal._
  def evaluate(debug: Boolean = false): Stream =
    Goal.evaluate(self, State.empty, debug)

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
  val always: Goal = equiv(Term.LValue("_always"), Term.LValue("_always"))

  val never: Goal = equiv(Term.LValue("_never"), Term.LValue("_never2"))

  def all(goals: List[Goal]): Goal = goals match {
    case Nil           => always
    case goal :: goals => goals.fold(goal)(conj(_, _))
  }

  def any(goals: List[Goal]): Goal = goals match {
    case Nil           => never
    case goal :: goals => goals.fold(goal)(disj(_, _))
  }

  def callFresh(f: Var => Goal): Goal = Fresh(f)
  def conj(goal1: Goal, goal2: Goal): Goal = Conj(goal1, goal2)
  def delay(g: => Goal): Goal = Delay(() => g)
  def disj(goal1: Goal, goal2: Goal): Goal = Disj(goal1, goal2)
  def equiv(term1: Term, term2: Term): Goal = Equiv(term1, term2)
  def extend(v: Var, t: Term, s: State): State =
    s.copy(substitution = s.substitution + (v -> t))

  private[kanren4s] def evaluate(
      goal: Goal,
      state: State,
      debug: Boolean = false
  ): Stream = {
    if (debug) {
      val varState =
        state.substitution.toSeq.map { case (v, t) => s"$v: $t" }.mkString(",")
      println(s"Evaluating: $goal ($varState)")
    }
    goal match {

      case Equiv(t1, t2) =>
        unify(t1, t2, state) match {
          case None => Stream.empty
          case Some(s) =>
            Stream.Nonempty(s, Stream.empty)
        }
      case Disj(a, b) =>
        Stream.union(evaluate(a, state, debug), evaluate(b, state, debug))
      case Conj(g1, g2) =>
        Stream.bind(evaluate(g1, state, debug), evaluate(g2, _, debug))
      case Fresh(goal) =>
        val newVar = state.variableCounter
        Stream.Procedure(() => evaluate(goal(newVar), state.nextVar, debug))
      case Delay(g) => Stream.Procedure(() => evaluate(g(), state, debug))
    }
  }

  def unify(t1: Term, t2: Term, s: State): Option[State] = {
    val u = walk(t1, s)
    val v = walk(t2, s)

    (u, v) match {
      case (Term.Variable(u), Term.Variable(v)) if u == v => Some(s)
      case (Term.Variable(u), _)            => Some(extend(u, v, s))
      case (_, Variable(v))                 => Some(extend(v, u, s))
      case (Term.LValue(u), Term.LValue(v)) => if (u == v) Some(s) else None
      case (u @ Term.Pair(x, y), v @ Term.Pair(x1, y1)) =>
        unify(x, x1, s) match {
          case Some(s1) => unify(y, y1, s1)
          case None     => None
        }
      case _ => None
    }
  }

  def walk(u: Term, s: State): Term = u match {
    case term @ Variable(u) =>
      s.substitution.get(u) match {
        case Some(u) => walk(u, s)
        case None    => term
      }
    case _ => u
  }

  private final case class Equiv(a: Term, b: Term) extends Goal
  private final case class Disj(g1: Goal, g2: Goal) extends Goal
  private final case class Conj(g1: Goal, g2: Goal) extends Goal
  private final case class Fresh(get: Var => Goal) extends Goal
  private final case class Delay(g: () => Goal) extends Goal
}
