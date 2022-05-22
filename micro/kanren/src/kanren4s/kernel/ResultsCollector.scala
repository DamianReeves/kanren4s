package kanren4s.kernel

trait ResultsCollector {
  type Stream
  def isEmpty(s: Stream): Boolean
  def append(s1: Stream, s2: Stream): Stream
  def bind[Col <: ResultsCollector.OfType[Stream]](s: Stream, goal: Goal)(
      settings: GoalEvalSettings[Stream, Col]
  ): Stream
  def mzero: Stream
  def mkStream(states: State*): Stream
  final def single(state: State): Stream = mkStream(state)

}

object ResultsCollector {
  type OfType[T] = ResultsCollector { type Stream = T }

  type DefaultStreamType = LazyList[State]

  type Default = ResultsCollector.OfType[DefaultStreamType]
  val Default: Default = new ResultsCollector {
    final override type Stream = DefaultStreamType

    def append(s1: Stream, s2: Stream): Stream = s1 ++ s2

    def bind[Col <: ResultsCollector.OfType[Stream]](s: Stream, goal: Goal)(
        settings: GoalEvalSettings[Stream, Col]
    ): Stream = {
      val run = goal(_: State, settings)
      s.flatMap(run)
    }
    final override def isEmpty(s: Stream): Boolean = s.isEmpty
    override val mzero: Stream = LazyList.empty
    override def mkStream(states: State*): Stream = LazyList(states: _*)
  }
}
