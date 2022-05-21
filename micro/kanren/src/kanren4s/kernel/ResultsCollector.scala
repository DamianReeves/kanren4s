package kanren4s.kernel

trait ResultsCollector {
  type Stream
  def mzero: Stream
  def mplus(s1: Stream, s2: Stream): Stream
  def mkStream(states: State*): Stream
  final def single(state: State): Stream = mkStream(state)
}

object ResultsCollector {
  type OfType[T] = ResultsCollector { type Stream = T }

  type DefaultStreamType = LazyList[State]

  type Default = ResultsCollector.OfType[DefaultStreamType]
  val Default: Default = new ResultsCollector {
    final override type Stream = DefaultStreamType
    override def mzero: Stream = LazyList.empty
    override def mplus(s1: Stream, s2: Stream): Stream = s1 ++ s2
    override def mkStream(states: State*): Stream = LazyList(states: _*)
  }
}
