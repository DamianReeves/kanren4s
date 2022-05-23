package kanren4s.kernel
import kanren4s.kernel.ResultStream.Empty
import kanren4s.kernel.ResultStream.Mature
import kanren4s.kernel.ResultStream.Immature

sealed abstract class ResultStream extends Product with Serializable { self =>
  import ResultStream._

  def ++(that: ResultStream): ResultStream = self match {
    case Empty => that
    // Equivalent to the procedure? case in microKanren
    case Immature(run)      => Immature(() => that ++ run())
    case Mature(head, tail) => Mature(head, tail ++ that)
  }

  def bind(goal: Goal): ResultStream = self match {
    case Empty              => Empty
    case Immature(run)      => Immature(() => run().bind(goal))
    case Mature(head, tail) => Mature(head, tail.bind(goal))
  }

  def peel: Option[(State, ResultStream)] = {
    def loop(stream: ResultStream): Option[(State, ResultStream)] =
      stream match {
        case Empty              => None
        case Immature(run)      => loop(run())
        case Mature(head, tail) => Some((head, tail))
      }
    loop(self)
  }

  def size: Int = toLazyList.size

  def take(n: Int): List[State] = {
    def loop(stream: ResultStream, n: Int, acc: List[State]): List[State] =
      if (n == 0) acc
      else
        stream match {
          case Empty              => acc
          case Immature(run)      => loop(run(), n, acc)
          case Mature(head, tail) => loop(tail, n - 1, head :: acc)
        }
    loop(self, n, Nil)
  }

  def toLazyList: LazyList[State] = {
    def loop(stream: ResultStream, acc: LazyList[State]): LazyList[State] =
      stream match {
        case Empty              => acc
        case Immature(run)      => loop(run(), acc)
        case Mature(head, tail) => loop(tail, head #:: acc)
      }
    loop(self, LazyList.empty)
  }

  def toList: List[State] = {
    def loop(stream: ResultStream, acc: List[State]): List[State] =
      stream match {
        case Empty              => acc
        case Immature(run)      => loop(run(), acc)
        case Mature(head, tail) => loop(tail, head :: acc)
      }
    loop(self, Nil)
  }

}
object ResultStream {

  val empty: ResultStream = Empty
  def append(left: ResultStream, right: ResultStream): ResultStream =
    left ++ right
  def bind(stream: ResultStream, goal: Goal): ResultStream =
    stream.bind(goal)
  def both(a: State, b: State): ResultStream = Mature(a, Mature(b, Empty))
  def cons(head: State, tail: ResultStream): ResultStream = Mature(head, tail)
  def list(state: State*): ResultStream = state.foldRight(empty)(cons)
  def single(state: State): ResultStream = Mature(state, Empty)
  def suspend(run: () => ResultStream): ResultStream = Immature(run)

  private[kanren4s] case object Empty extends ResultStream
  private[kanren4s] final case class Mature(head: State, tail: ResultStream)
      extends ResultStream
  private[kanren4s] final case class Immature(run: () => ResultStream)
      extends ResultStream
}
