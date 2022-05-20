package kanren4s.micro

final case class Var(variableCount: Int) {
  def next: Var = Var(variableCount + 1)
}

object Var {
  val default: Var = Var(0)
  val zero: Var = Var(0)
  @inline def incr(v: Var): Var = v.next

  def make(n: Int): Var = new Var(n)
}
