package kanren4s.micro
import scala.annotation.tailrec
sealed trait Stream extends Product with Serializable { self =>
  import Stream._

  def bind(f: State => Stream): Stream = self match {
    case Empty                => empty
    case Procedure(s)         => Procedure(() => s().bind(f))
    case Nonempty(head, tail) => f(head) union tail.bind(f)
  }

  def peel: Option[(Map[Var, Term], Stream)] = self match {
    case Empty                => None
    case Procedure(s)         => s().peel
    case Nonempty(head, tail) => Some((head.substitution, tail))
  }

  def take(n: Int): List[Map[Var, Term]] = {
    def loop(
        acc: List[Map[Var, Term]],
        n: Int,
        s: Stream
    ): List[Map[Var, Term]] =
      if (n == 0) {
        acc
      } else {
        s match {
          case Empty        => acc
          case Procedure(s) => loop(acc, n, s())
          case Nonempty(head, tail) =>
            loop(head.substitution :: acc, n - 1, tail)
        }
      }

    loop(Nil, n, self).reverse
  }

  def toList: List[Map[Var, Term]] = {
    @tailrec
    def loop(stream: Stream, acc: List[Map[Var, Term]]): List[Map[Var, Term]] =
      stream match {
        case Empty                => acc
        case Nonempty(head, tail) => loop(tail, head.substitution :: acc)
        case Procedure(s)         => loop(s(), acc)
      }

    loop(self, Nil).reverse
  }

  def union(that: Stream): Stream = self match {
    case Empty                => that
    case Procedure(s)         => Procedure(() => that union (s()))
    case Nonempty(head, tail) => Nonempty(head, that union (tail))
  }

}
object Stream {
  private[kanren4s] val empty: Stream = Stream.Empty

  def bind(s: Stream, g: State => Stream): Stream = s.bind(g)
  def peel(s: Stream): Option[(Map[Var, Term], Stream)] = s.peel
  def take(s: Stream, n: Int): List[Map[Var, Term]] = s.take(n)
  def toList(s: Stream): List[Map[Var, Term]] = s.toList
  def union(s1: Stream, s2: Stream): Stream = s1.union(s2)

  private[kanren4s] case object Empty extends Stream
  private[kanren4s] final case class Procedure(f: () => Stream) extends Stream
  private[kanren4s] final case class Nonempty(head: State, tail: Stream)
      extends Stream
}
