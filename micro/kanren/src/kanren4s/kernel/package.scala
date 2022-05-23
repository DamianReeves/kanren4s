package kanren4s
import com.softwaremill.tagging._

package object kernel {
  type Term = Any
  type VariableId = VariableId.Type

  implicit class VariableIdOps(val self: VariableId) extends AnyVal {
    def equalTo(that: Int): Boolean = self == VariableId.from(that)
    def next: VariableId = VariableId.next(self)
    def nextVariable: Variable = next.toVariable
    def toVariable: Variable = Variable(self, None)
  }
}
