package kanren4s.kernel

sealed trait Term extends Product with Serializable { self => }
object Term {
  def fromValue[A](a: => A): Term = Value(a)
  // def variable(name: String): Term = Variable.Named(name)
  type Variable = kanren4s.kernel.Variable
  val Variable: kanren4s.kernel.Variable.type = kanren4s.kernel.Variable
  private[kernel] final case class Value[+A](value: A) extends Term
  private[kernel] final case class Pair(left: Term, right: Term) extends Term
}
sealed trait Variable extends Term { self =>
  import Variable._
  def name: String
  def id: Id
  def isAnonymous: Boolean = self match {
    case Anonymous(_) => true
    case _            => false
  }

  override def toString(): String = self match {
    case Anonymous(id)   => s"#$id"
    case Named(name, id) => s"$name(#$id)"
  }
}

object Variable {
  type Id = Int
  val default: Variable = Anonymous(0)
  val first: Variable = default

  def unapply(term: Term): Option[(Variable, Id, String)] = term match {
    case v @ Anonymous(_) => Some((v, v.id, v.name))
    case v @ Named(_, _)  => Some((v, v.id, v.name))
    case _                => None
  }

  private[kernel] final case class Anonymous(id: Id) extends Variable {
    def name: String = s"#$id"
  }

  private[kernel] final case class Named(name: String, id: Id) extends Variable
}
