package kanren4s.core
import com.softwaremill.tagging._

object predef {

  type SeqNum = Int @@ SeqNum.Tag
  object SeqNum {
    sealed trait Tag
    val zero: SeqNum = wrap(0)
    def fromInt(i: Int): SeqNum = wrap(i)
    private[predef] def wrap(n: Int): SeqNum = n.taggedWith[Tag]
  }

  implicit final class SeqNumOps(val self: SeqNum) extends AnyVal {
    def next: SeqNum = SeqNum.wrap(self + 1)
  }

  final type Substitution[+A] = Map[Var, A]
  object Substitution {
    def empty[A]: Substitution[A] = Map.empty
    def apply[A](pairs: (Var, A)*): Substitution[A] = Map(pairs: _*)

  }

  sealed trait LObject
  case class Var(n: SeqNum, label: Option[String]) extends LObject { self =>

    /** Alias for `withLabel`.
      */
    def ??(label: String): Var = withLabel(label)

    def name: String = label match {
      case None       => s"#$n"
      case Some(name) => name
    }

    /** Alias for `withLabel`.
      */
    def labeled(label: String): Var = withLabel(label)

    def next: Var = Var(n.next, None)
    def next(label: String): Var = Var(n.next, Option(label))

    def withLabel(label: String): Var = copy(label = Option(label))

    override def toString: String = label match {
      case None       => s"#$n"
      case Some(name) => s"$name(#$n)"
    }
  }
  object Var {
    val default: Var = anonymous(SeqNum.zero)
    val first: Var = default

    def apply(n: Int): Var = Var(SeqNum.fromInt(n), None)

    def anonymous(seqNum: SeqNum = SeqNum.zero): Var =
      Var(seqNum, None)

    def labeled(label: String, seqNum: SeqNum = SeqNum.zero): Var =
      Var(seqNum, Option(label))
  }
}
