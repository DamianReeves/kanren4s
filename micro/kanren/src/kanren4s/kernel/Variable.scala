package kanren4s.kernel

final case class Variable(id: VariableId, label: Option[String]) {
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
