package kanren4s.kernel

final case class Var(id: VariableId, label: Option[String]) {
  self =>

  /** Alias for `withLabel`.
    */
  def ??(label: String): Var = withLabel(label)

  def name: String = label match {
    case None       => s"#$id"
    case Some(name) => name
  }

  /** Alias for `withLabel`.
    */
  def labeled(label: String): Var = withLabel(label)

  def next: Var = id.nextVariable
  def next(label: String): Var = Var(id.next, Option(label))

  def withLabel(label: String): Var = copy(label = Option(label))

  override def toString: String = label match {
    case None       => s"#$id"
    case Some(name) => s"$name(#$id)"
  }
}

object Var {
  val default: Var = anonymous(VariableId.first)
  val first: Var = default

  def anonymous(id: VariableId = VariableId.first): Var =
    Var(id, None)

  def at(idx: Int): Var = anonymous(VariableId.from(idx))
  def labeled(name: String, id: VariableId = VariableId.first): Var =
    Var(id, Some(name))
}
