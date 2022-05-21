package kanren4s.kernel
import com.softwaremill.tagging._

object VariableId {
  type Type = Int @@ VarId
  trait VarId
  val first: VariableId = 0.taggedWith[VarId]
  def next(current: VariableId): VariableId = (current + 1).taggedWith[VarId]
  def from(id: Int): VariableId = id.taggedWith[VarId]
}
