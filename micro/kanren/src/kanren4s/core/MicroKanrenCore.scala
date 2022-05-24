package kanren4s.core
import com.softwaremill.tagging._

trait MicroKanrenCore extends GoalModule { self =>

  def callFresh(f: Var => Goal): Goal = Goal.callFresh(f)
  def eq(x: Term, y: Term): Goal = Goal.eq(x, y)
  def conj(g1: Goal, g2: Goal): Goal = Goal.and(g1, g2)
  def disj(g1: Goal, g2: Goal): Goal = Goal.or(g1, g2)
  def goalFail: Goal = Goal.fail
  def goalSucceed: Goal = Goal.succeed

  // def pull(s: ResultStream): LazyList[State] = {
  //   def loop(stream: ResultStream, acc: LazyList[State]): LazyList[State] =
  //     stream match {
  //       case ResultStream.Empty              => acc
  //       case ResultStream.Immature(run)      => loop(run(), acc)
  //       case ResultStream.Mature(head, tail) => loop(tail, head #:: acc)
  //     }
  //   loop(s, LazyList.empty)
  // }

  implicit class TermOps(val self: Term) {
    def ===(that: Term): Goal = Goal.eq(self, that)
  }

}
