package kanren4s.micro

sealed trait Stream extends Product with Serializable { self =>
  import Stream._

  def union(that: Stream): Stream = ???

}
object Stream {
  private[kanren4s] val empty: Stream = Stream.Empty

  private case object Empty extends Stream
  private final case class Procedure(f: () => Stream) extends Stream
  private final case class Nonempty(head: State, tail: Stream) extends Stream
}
