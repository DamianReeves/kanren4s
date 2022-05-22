package kanren4s.kernel

sealed trait Term extends Product with Serializable { self => }
object Term {
  val empty: Term = Empty
  def apply[A](a: A): Term = a match {
    case a: Term => a
    case a       => fromValue(a)
  }

  def cons[A, B](pair: (A, B)): Term = {
    val t1 = Term(pair._1)
    val t2 = Term(pair._2)
    Pair(t1, t2)
  }

  def fromValue[A](a: => A): Term = Value(a)
  def fromPair(pair: (Term, Term)): Term = Pair(pair._1, pair._2)
  def list(terms: Term*): Term = terms.foldRight(Empty: Term)(Pair(_, _))

  // def variable(name: String): Term = Variable.Named(name)
  type Variable = kanren4s.kernel.Variable
  val Variable: kanren4s.kernel.Variable.type = kanren4s.kernel.Variable
  private[kernel] case object Empty extends Term
  private[kernel] final case class Value[+A](value: A) extends Term
  private[kernel] final case class Pair(left: Term, right: Term) extends Term
  // private[kernel] final case class Func(apply:Term => Term) extends Term
}

final case class Variable(id: VariableId, label: Option[String]) extends Term {
  self =>

  /** Alias for `withLabel`.
    */
  def ??(label: String): Variable = withLabel(label)

  def name: String = label match {
    case None       => s"#$id"
    case Some(name) => name
  }

  /** Alias for `withLabel`.
    */
  def labeled(label: String): Variable = withLabel(label)

  def next: Variable = id.nextVariable
  def next(label: String): Variable = Variable(id.next, Option(label))

  def withLabel(label: String): Variable = copy(label = Option(label))

  override def toString: String = label match {
    case None       => s"#$id"
    case Some(name) => s"$name(#$id)"
  }
}

object Variable {
  val default: Variable = anonymous(VariableId.first)
  val first: Variable = default

  def anonymous(id: VariableId = VariableId.first): Variable =
    Variable(id, None)

  def at(idx: Int): Variable = anonymous(VariableId.from(idx))
  def labeled(name: String, id: VariableId = VariableId.first): Variable =
    Variable(id, Some(name))
}
