package kanren4s.kernel
import kanren4s.kernel.Goal.Eq

trait GoalRunner {

  type StreamMod <: StreamModule
  val StreamMod: StreamMod
  final type Stream = StreamMod.Stream

  private def eq(t1: Term, t2: Term)(state: State): Stream = {
    state.substitutions.unify(t1, t2) match {
      case None        => StreamMod.mzero
      case Some(value) => StreamMod.mkStream(state.withSubstitutions(value))
    }
  }

  final def run(goal: Goal, state: State): Stream = {
    goal match {
      case Goal.Eq(a, b) => eq(a, b)(state)
      case _             => StreamMod.single(state)
    }
  }
  def settings: GoalEvaluationSettings

}

object GoalRunner {
  val default = new GoalRunner {

    type StreamMod = StreamModule.OfType[StreamModule.DefaultStream]
    val StreamMod: StreamMod = StreamModule.default

    override def settings: GoalEvaluationSettings =
      GoalEvaluationSettings.default
  }
}

trait StreamModule {
  type Stream
  def mzero: Stream
  def mplus(s1: Stream, s2: Stream): Stream
  def mkStream(states: State*): Stream
  final def single(state: State): Stream = mkStream(state)
}

object StreamModule {
  type OfType[T] = StreamModule { type Stream = T }

  type DefaultStream = LazyList[State]

  val default: StreamModule.OfType[DefaultStream] = new StreamModule {
    override type Stream = DefaultStream
    override def mzero: Stream = LazyList.empty
    override def mplus(s1: Stream, s2: Stream): Stream = s1 ++ s2
    override def mkStream(states: State*): Stream = LazyList(states: _*)
  }
}
