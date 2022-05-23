package kanren4s.kernel
import scala.annotation.implicitNotFound
import scala.{math => sm}

/** `Equal[A]` provides implicit evidence that two values of type `A` can be
  * compared for equality.
  */
@implicitNotFound("No implicit Equal defined for ${A}.")
trait Equal[-A] { self =>
  final def equal(l: A, r: A): Boolean =
    Equal.refEq(l, r) || checkEqual(l, r)

  protected def checkEqual(l: A, r: A): Boolean

  /** Returns whether two values of type `A` are not equal.
    */
  final def notEqual(l: A, r: A): Boolean =
    !equal(l, r)

  def toScala[A1 <: A]: sm.Equiv[A1] = self.equal(_, _)
}

object Equal {

  /** Summons an implicit `Equal[A]`.
    */
  def apply[A](implicit equal: Equal[A]): Equal[A] =
    equal

  /** Constructs an `Equal[A]` that uses the default notion of equality embodied
    * in the implementation of `equals` for values of type `A`.
    */
  def default[A]: Equal[A] =
    DefaultEqual

  def fromScala[A](implicit equiv: sm.Equiv[A]): Equal[A] = equiv.equiv(_, _)

  /** Constructs an `Equal[A]` from a function. The instance will be optimized
    * to first compare the values for reference equality and then compare the
    * values for value equality.
    */
  def make[A](equal: (A, A) => Boolean): Equal[A] =
    (l, r) => equal(l, r)

  /** Returns whether two values refer to the same location in memory.
    */
  private[kernel] def refEq[A](l: A, r: A): Boolean =
    l.asInstanceOf[AnyRef] eq r.asInstanceOf[AnyRef]

  /** An `Equal` instance for `Any` values that uses Scala's default notion of
    * equality embodied in `equals`.
    */
  private lazy val DefaultEqual: Equal[Any] =
    Equal.make(_ == _)

  implicit val IntEqual: Equal[Int] =
    Equal.make(_ == _)

}
