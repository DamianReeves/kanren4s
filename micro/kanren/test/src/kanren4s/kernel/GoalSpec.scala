package kanren4s.kernel

import zio.test._
import zio.test.TestAspect.{ignore, tag}
import Term.cons

object GoalSpec extends DefaultRunnableSpec {
  def spec = suite("Goal Spec")(
    suite("Eq")(
      test("eq should return a stream of answers") {
        val a = Variable.at(0) ?? "a"
        val b = Variable.at(1) ?? "b"
        val cat = Term.fromValue("cat")
        val t1 = cons(cons(cat -> a) -> b)
        val t2 = cons(cons("cat" -> "horse") -> "turtle")
        val state = State.empty.withNextIndex(2)
        val goal = Goal.eq(t1, t2)
        val actual = goal(state)
        println(s"Results: ${actual.toList}")
        assertTrue(actual.size == 1, actual.head.nextVariableId equalTo 2)
      }
    ),
    suite("Fresh")(
      test("Fresh provides fresh variables") {
        var actual: Variable = null
        Goal.fresh { x =>
          actual = x
          Goal.eq(x, x)
        }
        assertTrue(actual != null)
      } @@ ignore @@ tag("Not Ready")
    )
  )
}
