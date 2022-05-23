package kanren4s
import kanren4s.core._

object ukanren
    extends UserInterface
    with StateModule
    with GoalModule
    with MicroKanrenCore {}
