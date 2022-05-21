package kanren4s.kernel

import zio.test._

object GoalSpec extends DefaultRunnableSpec {
  def spec = suite("Goal Spec")(
    suite("Fresh")(
      test("Fresh provides fresh variables") {
        val actual = Goal.fresh(x => ???)
        assertTrue(actual == ???)
      }
    )
  )
}
