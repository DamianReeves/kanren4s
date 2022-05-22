package kanren4s.kernel
import kanren4s.kernel.ResultStream.Empty
import kanren4s.kernel.ResultStream.Cons
import kanren4s.kernel.ResultStream.Suspend

sealed abstract class ResultStream extends Product with Serializable { self =>
  import ResultStream._

  def ++(that: ResultStream): ResultStream = self match {
    case Empty => that
    // Equivalent to the procedure? case in microKanren
    case Suspend(run)     => Suspend(() => that ++ run())
    case Cons(head, tail) => Cons(head, tail ++ that)
  }

  def bind(goal: Goal): ResultStream = self match {
    case Empty            => Empty
    case Suspend(run)     => Suspend(() => run().bind(goal))
    case Cons(head, tail) => Cons(head, tail.bind(goal))
  }

  def peel: Option[(State, ResultStream)] = {
    def loop(stream: ResultStream): Option[(State, ResultStream)] =
      stream match {
        case Empty            => None
        case Suspend(run)     => loop(run())
        case Cons(head, tail) => Some((head, tail))
      }
    loop(self)
  }

  def size: Int = toLazyList.size

  def take(n: Int): List[State] = {
    def loop(stream: ResultStream, n: Int, acc: List[State]): List[State] =
      if (n == 0) acc
      else
        stream match {
          case Empty            => acc
          case Suspend(run)     => loop(run(), n, acc)
          case Cons(head, tail) => loop(tail, n - 1, head :: acc)
        }
    loop(self, n, Nil)
  }

  def toLazyList: LazyList[State] = self match {
    case Empty        => LazyList.empty
    case Suspend(run) => run().toLazyList
    case Cons(head, tail) =>
      def loop(stream: ResultStream): LazyList[State] = stream match {
        case Empty            => LazyList.empty
        case Suspend(run)     => loop(run())
        case Cons(head, tail) => head #:: loop(tail)
      }
      loop(tail)
  }

  def toList: List[State] = {
    def loop(stream: ResultStream, acc: List[State]): List[State] =
      stream match {
        case Empty            => acc
        case Suspend(run)     => loop(run(), acc)
        case Cons(head, tail) => loop(tail, head :: acc)
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
  def both(a: State, b: State): ResultStream = Cons(a, Cons(b, Empty))
  def cons(head: State, tail: ResultStream): ResultStream = Cons(head, tail)
  def list(state: State*): ResultStream = state.foldRight(empty)(cons)
  def single(state: State): ResultStream = Cons(state, Empty)
  def suspend(run: () => ResultStream): ResultStream = Suspend(run)

  private[kanren4s] case object Empty extends ResultStream
  private[kanren4s] final case class Cons(head: State, tail: ResultStream)
      extends ResultStream
  private[kanren4s] final case class Suspend(run: () => ResultStream)
      extends ResultStream
}
