package kanren4s.kernel

import zio.test._

object GoalSpec extends DefaultRunnableSpec {
  def spec = suite("Goal Spec")(
    suite("Fresh")(
      test("Fresh provides fresh variables") {
        var actual:Variable = null
        Goal.fresh{x =>
          actual = x
          Goal.eq(x, x)
        }
        assertTrue(actual != null)
      }
    )
  )
}
