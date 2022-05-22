package kanren4s.kernel

trait ResultsCollector {
  type Stream
  def isEmpty(s: Stream): Boolean
  def append(s1: Stream, s2: Stream): Stream
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

    def append(s1: Stream, s2: Stream): Stream = s1 ++ s2
    final override def isEmpty(s: Stream): Boolean = s.isEmpty
    override val mzero: Stream = LazyList.empty
    override def mplus(s1: Stream, s2: Stream): Stream = s1 ++ s2
    override def mkStream(states: State*): Stream = LazyList(states: _*)
  }
}
