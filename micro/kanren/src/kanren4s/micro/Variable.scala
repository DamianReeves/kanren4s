package kanren4s.micro

final case class Variable private[micro] (variableCount: Int)

object Variable {
  def incr(v: Variable): Variable = Variable(v.variableCount + 1)

  def make(n: Int = 0): Variable = new Variable(n)
}
