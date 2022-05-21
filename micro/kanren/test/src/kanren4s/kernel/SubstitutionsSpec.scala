package kanren4s.kernel
import zio.test._

object SubstitutionsSpec extends DefaultRunnableSpec {
  def spec = suite("Substitutions Spec")(
    suite("Given v ≡ 42") {
      val v = Variable.named("v")
      val `given v ≡ 42` =
        Substitutions.setupUnchecked(v -> Term.fromValue(42))
      suite("When we do a walk on v")(
        test("Then we should get 42") {
          val sut = `given v ≡ 42`
          val actual = sut.walk(v)
          assertTrue(actual == Term.fromValue(42))
        }
      ) + suite("and u ≡ v") {
        val u = v.next("u")
        val `and u ≡ v` = `given v ≡ 42` andAlsoGiven (u, v)
        suite("When we do a walk on u")(
          test("Then we should get 42") {
            val sut = `and u ≡ v`
            val actual = sut.walk(u)
            assertTrue(actual == Term.fromValue(42))
          }
        )
      }
    }
  )
}
