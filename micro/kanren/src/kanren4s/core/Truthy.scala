package kanren4s.core

import com.softwaremill.tagging._

trait Truthy[-A] {
  final def apply(a: A): Boolean = isTruthy(a)
  def isTruthy(a: A): Boolean
}

object Truthy extends LowPriorityTruthy {
  def apply[A](implicit ev: Truthy[A]): Truthy[A] = ev

  implicit val TruthyBoolean: Truthy[Boolean] = new Truthy[Boolean] {
    override def isTruthy(a: Boolean): Boolean = a
  }

  implicit final class TruthyOps[A](val self: A) extends AnyVal {
    def isTruthy(implicit ev: Truthy[A]): Boolean = ev(self)
  }
}

trait LowPriorityTruthy {
  implicit def defaultTruthy[A]: Truthy[A] = new Truthy[A] {
    override def isTruthy(a: A): Boolean = a match {
      case false => false
      case _     => true
    }
  }
}
