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
  def labelled(label: String): Variable = withLabel(label)

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
  def named(name: String, id: VariableId = VariableId.first): Variable =
    Variable(id, Some(name))
}
