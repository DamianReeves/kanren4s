package kanren4s.core

trait UserInterface extends MicroKanrenCore {

  def callInitialState(n: Int, goal: Goal): List[State] =
    take(n, pull(goal(State.empty)))

  def callInitialState(n: Option[Int], goal: Goal): List[State] =
    take(n, pull(goal(State.empty)))

  def exists(f: () => Goal): Goal = f()
  def exists(f: (Var) => Goal): Goal = callFresh(f)
  def exists(f: (Var, Var) => Goal): Goal = callFresh(q => exists(f(q, _)))
  def exists(f: (Var, Var, Var) => Goal): Goal =
    callFresh(q => exists(f(q, _, _)))
  def exists(f: (Var, Var, Var, Var) => Goal): Goal =
    callFresh(q => exists(f(q, _, _, _)))
  def exists(f: (Var, Var, Var, Var, Var) => Goal): Goal =
    callFresh(q => exists(f(q, _, _, _, _)))
  def exists(f: (Var, Var, Var, Var, Var, Var) => Goal): Goal =
    callFresh(q => exists(f(q, _, _, _, _, _)))
  def exists(f: (Var, Var, Var, Var, Var, Var, Var) => Goal): Goal =
    callFresh(q => exists(f(q, _, _, _, _, _, _)))
  def exists(f: (Var, Var, Var, Var, Var, Var, Var, Var) => Goal): Goal =
    callFresh(q => exists(f(q, _, _, _, _, _, _, _)))

  def pull(stream: StateStream): StateStream = stream.pull

  def snooze(goal: => Goal): Goal = Goal.snooze(goal)

  def take(n: Int, stream: StateStream): List[State] =
    take(if (n < 0) None else Some(n), stream)

  def take(n: Option[Int], stream: StateStream): List[State] = n match {
    case None    => stream.takeAll
    case Some(n) => stream.take(n)
  }

}
