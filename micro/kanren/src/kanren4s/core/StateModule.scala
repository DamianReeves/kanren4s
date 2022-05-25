package kanren4s.core
import scala.annotation.tailrec
trait StateModule extends SubstitutionModule {
  val emptyState: State = State.empty

  case class State(substitution: Substitution, seqNum: SeqNum) { self =>

    def freshVariable(): (Var, State) =
      (Var.anonymous(seqNum), withNextVariableId(seqNum.next))

    def freshVariable(label: String): (Var, State) =
      (Var.labeled(label, seqNum), withNextVariableId(seqNum.next))

    def withSeqNum(i: Int): State =
      copy(seqNum = SeqNum.fromInt(i))
    def withNextVariableId(seqNum: SeqNum): State =
      copy(seqNum = seqNum)
    def withSubstitution(substitution: Substitution): State =
      copy(substitution = substitution)
    def withSubstitutions(bindings: (Var, Term)*): State =
      copy(substitution = bindings.toMap)

    override def toString: String =
      s"($substitution, $seqNum"
  }
  object State {
    val empty: State = State(Substitution.empty, SeqNum.zero)
    def withSubstitution(s: Substitution): State = State(s, SeqNum.zero)
    def withSubstitutions(substitutions: (Var, Term)*): State =
      State(substitutions.toMap, SeqNum.zero)
  }

  sealed abstract class StateStream extends Product with Serializable { self =>
    import StateStream._
    def ++(that: StateStream): StateStream = self match {
      case Empty => that
      // Equivalent to the procedure? case in microKanren
      case Immature(run) =>
        StateStream.suspend(() => that ++ run())
      case Mature(head, tail) => Mature(head, tail ++ that)
    }

    def bind(f: State => StateStream): StateStream = {
      @tailrec
      def loop(curr: StateStream, acc: StateStream): StateStream = curr match {
        case Empty => acc
        case Immature(run) =>
          StateStream.suspend(() => run().bind(f))
        case Mature(head, tail) =>
          loop(tail, f(head) ++ acc)
      }

      loop(self, Empty)
    }

    def cdr: StateStream = peel.map(_._2).getOrElse(Empty)

    def headOption: Option[State] = peel.map(_._1)

    def peel: Option[(State, StateStream)] = {
      @tailrec
      def loop(stream: StateStream): Option[(State, StateStream)] =
        stream match {
          case Empty              => None
          case Immature(run)      => loop(run())
          case Mature(head, tail) => Some((head, tail))
        }
      loop(self)
    }

    def pull: StateStream = {
      @tailrec
      def loop(s: StateStream): StateStream =
        s match {
          case Immature(run) => loop(run())
          case stream        => stream
        }

      loop(self)
    }

    def size: Int = toLazyList.size

    def take(n: Int): List[State] = {
      @tailrec
      def loop(stream: StateStream, n: Int, acc: List[State]): List[State] =
        if (n == 0) acc
        else
          stream match {
            case Empty              => acc
            case Immature(run)      => loop(run(), n, acc)
            case Mature(head, tail) => loop(tail, n - 1, head :: acc)
          }
      loop(self, n, Nil)
    }

    def takeAll: List[State] = {
      @tailrec
      def loop(stream: StateStream, acc: List[State]): List[State] =
        stream match {
          case Empty              => acc
          case Immature(run)      => loop(run(), acc)
          case Mature(head, tail) => loop(tail, head :: acc)
        }
      loop(self, Nil)
    }

    // def take(n: Option[Int]): List[State] = {
    //   @tailrec
    //   def loop(
    //       stream: StateStream,
    //       n: Option[Int],
    //       acc: List[State]
    //   ): List[State] =
    //     if (n == Some(0)) acc
    //     else
    //       stream match {
    //         case Empty              => acc
    //         case Immature(run)      => loop(run(), n, acc)
    //         case Mature(head, tail) => loop(tail, n.map(_ - 1), head :: acc)
    //       }
    //   loop(self, n, Nil)
    // }

    def toLazyList: LazyList[State] = {
      def loop(stream: StateStream, acc: LazyList[State]): LazyList[State] =
        stream match {
          case Empty              => acc
          case Immature(run)      => loop(run(), acc)
          case Mature(head, tail) => loop(tail, head #:: acc)
        }
      loop(self, LazyList.empty)
    }

    def toList: List[State] = {
      def loop(stream: StateStream, acc: List[State]): List[State] =
        stream match {
          case Empty              => acc
          case Immature(run)      => loop(run(), acc)
          case Mature(head, tail) => loop(tail, head :: acc)
        }
      loop(self, Nil)
    }

  }
  object StateStream {

    val empty: StateStream = Empty
    def append(left: StateStream, right: StateStream): StateStream =
      left ++ right
    def apply(state: State*): StateStream = state.foldRight(empty)(cons)
    def bind(stream: StateStream, f: State => StateStream): StateStream =
      stream.bind(f)
    def both(a: State, b: State): StateStream = Mature(a, Mature(b, Empty))
    def cons(head: State, tail: StateStream): StateStream = Mature(head, tail)
    def list(state: State*): StateStream = state.foldRight(empty)(cons)
    def single(state: State): StateStream = Mature(state, Empty)
    def suspend(run: () => StateStream): StateStream = Immature(run)

    private[kanren4s] case object Empty extends StateStream
    private[kanren4s] case class Mature(head: State, tail: StateStream)
        extends StateStream
    private[kanren4s] case class Immature(run: () => StateStream)
        extends StateStream
  }

  trait StateLike { self =>
    def substitution: Substitution
    def seqNumn: SeqNum
  }
}
