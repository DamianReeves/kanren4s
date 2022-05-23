package kanren4s.kernel

import zio.test._

object MicroKanrenSpec extends DefaultRunnableSpec {
  import ukanren._
  def spec = suite("MicroKanren Spec")(
    suite("Unify")(
      test("Should produce an empty substitution when the values are equal") {
        val actual = unify(1337, 1337, Substitution.empty)
        assertTrue(
          actual == Some(Substitution.empty)
        )
      }
    )
  )
}
