package kanren4s.micro
import kanren4s.micro.{Var => VariableCount}

sealed trait Term extends Product with Serializable { self =>
  import Term._

  override def toString(): String = self match {
    case Term.Variable(VariableCount(v)) => s"x$v"
    case Term.LValue(value)              => value.toString
    case Term.Pair(l, r)                 => s"($l . $r)"
  }
}

object Term {
  private[kanren4s] final case class Variable(get: VariableCount) extends Term
  private[kanren4s] final case class LValue[+A](value: A) extends Term
  private[kanren4s] final case class Pair(l: Term, r: Term) extends Term
}
