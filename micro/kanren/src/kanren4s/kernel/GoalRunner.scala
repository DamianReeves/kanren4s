package kanren4s.kernel

trait GoalRunner {

  type Stream <: StreamModule
  val Stream: Stream

  def eq(t1: Term, t2: Term)(state: State) = {
    state.substitutions.unify(t1, t2) match {
      case None        => Stream.mzero
      case Some(value) => Stream.mkStream(state.withSubstitutions(value))
    }
  }

  final def run(goal: Goal, state: State): State = state
  def settings: GoalEvaluationSettings

}

object GoalRunner {
  val default = new GoalRunner {

    type Stream = StreamModule.OfType[StreamModule.DefaultStream]
    val Stream: Stream = StreamModule.default

    override def settings: GoalEvaluationSettings =
      GoalEvaluationSettings.default
  }
}

trait StreamModule {
  type Stream
  def mzero: Stream
  def mplus(s1: Stream, s2: Stream): Stream
  def mkStream(states: State*): Stream

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
