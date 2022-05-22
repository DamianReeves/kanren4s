package kanren4s.kernel
import zio.test._
import zio.test.TestAspect.{ignore, tag}
object SubstitutionsSpec extends DefaultRunnableSpec {
  def spec = suite("Substitutions Spec")(
    suite("Given v ≡ 42") {
      val v = Variable.labeled("v")
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
    } + suite("Unification")(
      test("Unifying an empty substitution with unifyable facts") {
        val v = Variable.at(0) ?? "v"
        val u = Variable.at(1) ?? "u"
        val cat = Term.fromValue("cat")
        val `v ≡ u` = Term.Pair(v, u)
        val `u ≡ cat` = Term.Pair(u, cat)
        val actual = Substitutions.empty.unify(`v ≡ u`, `u ≡ cat`)
        val expected = Substitutions.setupUnchecked(v -> u, u -> cat)
        assertTrue(actual == Some(expected))
      }
    ) + suite("Walking")(
      test("Walking through a substitution should succeed") {
        val a = Variable.at(0) ?? "a"
        val b = Variable.at(1) ?? "b"
        val c = Variable.at(2) ?? "c"
        val d = Variable.at(3) ?? "d"
        val e = Variable.at(4) ?? "e"
        val cat = Term.fromValue("cat")

        val sut = Substitutions.setupUnchecked(
          c -> d,
          b -> cat,
          a -> b
        )

        assertTrue(
          sut.walk(a) == cat,
          sut.valueOf(a) == sut.walk(a),
          sut.walk(b) == cat,
          sut.valueOf(b) == sut.walk(b),
          sut.walk(c) == d,
          sut.valueOf(c) == sut.walk(c),
          // Walk doesn't know anything extra about d (even though it would unify with c)
          sut.walk(d) == d,
          sut.valueOf(d) == sut.walk(d),
          // Walk knows nothing extra about e (we have no substitutions)
          sut.walk(e) == e,
          sut.valueOf(e) == sut.walk(e)
        )
      }
    )
  )
}
