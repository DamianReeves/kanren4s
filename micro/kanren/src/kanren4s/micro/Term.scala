package kanren4s.micro
import kanren4s.micro.{Variable => VariableCount}

sealed trait Term extends Product with Serializable { self =>
  import Term._

  override def toString(): String = self match {
    case Term.Variable(VariableCount(v)) => s"x$v"
    case Term.Symbol(name, args)         => s"$name[${args.mkString(", ")}]"
  }
}

object Term {
  private[kanren4s] final case class Variable(get: VariableCount) extends Term
  private[kanren4s] final case class Symbol(name: Any, args: List[Term])
      extends Term
}
