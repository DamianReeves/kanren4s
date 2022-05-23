package kanren4s.kernel

import zio.test._
import zio.test.TestAspect.{ignore, tag}

object GoalSpec extends DefaultRunnableSpec {
  def spec = suite("Goal Spec")(
    suite("Eq")(
      test("eq should return a stream of answers") {
        val a = Var(0) ?? "a"
        val b = Var(1) ?? "b"
        val cat = "cat"
        val t1 = ((cat -> a) -> b)
        val t2 = (("cat" -> "horse") -> "turtle")
        val state = State.empty.withNextIndex(2)
        val goal = Goal.eq(t1, t2)
        val actual = goal(state)
        val results = actual.take(2)
        println(s"Results: ${actual.toList}")
        assertTrue(
          actual.toList.size == 1,
          actual.toList.head.nextVariableId equalTo 2
        )
      }
    ),
    suite("Fresh")(
      test("Fresh provides fresh variables") {
        val state = State.empty
        val goal = Goal.fresh { (x, y) => Goal.eq(x, y) }
        val result = goal(state)
        assertTrue(
          result.size == 1,
          result.toLazyList.head.nextVariableId equalTo 2
        )
      }
    ),
    suite("Or/Disjunction")(
      test("Disjunction should work when it terminates") {
        val a = Var(0) ?? "a"
        val b = Var(1) ?? "b"
        val c = Var(2) ?? "c"
        val d = Var(3) ?? "d"
        val t1 = ((a -> b) -> c)
        val t2 = ((a -> d) -> c)
        val state = State.empty.withNextIndex(4)
        val goal = Goal.or(Goal.eq(t1, t2), Goal.eq(t2, t1))
        val actual = goal(state)
        val llResults = actual.toLazyList
        println(s"Results Disj: ${actual.toList}")
        assertTrue(actual.size == 2, llResults.head.nextVariableId equalTo 4)
      }
    )
  )
}
