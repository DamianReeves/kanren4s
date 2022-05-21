package kanren4s.kernel

import zio.test._
import Term.cons

object GoalRunnerSpec extends DefaultRunnableSpec {
  def spec = suite("GoalRunner Spec")(
    suite("eq")(
      test("eq should return a stream of answers") {
        val a = Variable.at(0) ?? "a"
        val b = Variable.at(1) ?? "b"
        val cat = Term.fromValue("cat")
        val t1 = cons(cons(cat -> a) -> b)
        val t2 = cons(cons("cat" -> "horse") -> "turtle")
        val state = State.empty.withNextIndex(2)
        val sut = GoalRunner.default
        val actual = sut.eq(t1, t2)(state)
        assertTrue(actual == sut.Stream.mzero)
      }
    )
  )
}
