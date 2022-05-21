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
  override def canEqual(obj: Any): Boolean =
    obj != null && obj.isInstanceOf[Variable]
  override def equals(obj: Any): Boolean =
    this.eq(obj.asInstanceOf[AnyRef]) || canEqual(obj) && {
      val other = obj.asInstanceOf[Variable]
      id == other.id
    }

  override def hashCode: Int = id.hashCode

  def name: String

  def id: Id

  def isAnonymous: Boolean = self match {
    case Anonymous(_) => true
    case _            => false
  }

  override def productArity: Int = 2
  override def productElement(n: Int): Any = n match {
    case 0 => self.id
    case 1 => self.name
    case n => throw new IndexOutOfBoundsException(n.toString)
  }

  override def toString(): String = self match {
    case Named(name, id) => s"$name(#$id)"
    case v   => s"#${v.id}"
  }
}

object Variable {
  type Id = Int
  val default: Variable = Anonymous(0)
  val first: Variable = default

  def unapply(term: Term): Option[(Variable, Id, String)] = term match {
    case v: Anonymous => Some((v, v.id, v.name))
    case v: Named     => Some((v, v.id, v.name))
    case _            => None
  }

  private[kernel] final class Anonymous(val id: Id) extends Variable {
    def name: String = s"#$id"
  }
  object Anonymous {
    def apply(id: Id): Anonymous = new Anonymous(id)
    def unapply(term: Term): Option[Id] = term match {
      case v: Anonymous => Some(v.id)
      case _            => None
    }
  }

  private[kernel] final class Named(val name: String, val id: Id) extends Variable
  object Named {
    def apply(name: String, id: Int): Named = new Named(name, id)
    def unapply(term: Term): Option[(String, Id)] = term match {
      case v: Named => Some((v.name, v.id))
      case _        => None
    }
  }
}
