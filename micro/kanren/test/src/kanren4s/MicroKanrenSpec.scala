package kanren4s

import zio.test._

object MicroKanrenSpec extends DefaultRunnableSpec {
  import ukanren._
  def spec = suite("MicroKanren Spec")(
    suite("Goals")(
      suite("eq/===")(
        test("1 ≡ 1") {
          val actual = (1 === 1)(emptyState)
          assertTrue(actual.headOption == Some(State.empty))
        },
        test("1 ≡ 2") {
          val actual = (1 === 2)(emptyState)
          assertTrue(actual == StateStream.empty)
        }
      ),
      suite("callFresh q ≡ 5")(
        test("callFresh") {
          val actual = callFresh(q => (q === 5))(emptyState)
          assertTrue(
            actual.headOption == Some(
              State(Substitution(Var(0) -> 5), SeqNum.fromInt(1))
            )
          )
        }
      )
    ),
    suite("Unify")(
      test("Should produce an empty substitution when the values are equal") {
        val actual = unify(1337, 1337, Substitution.empty)
        assertTrue(
          actual == Some(Substitution.empty)
        )
      },
      test("Should return None when given 2 non-equal values") {
        val actual = unify(1337, 1338, Substitution.empty)
        assertTrue(
          actual == None
        )
      },
      test(
        "Should return a single variable binding when a value is unified with a variable"
      ) {
        val actual = unify(1337, Var(0), Substitution.empty)
        assertTrue(
          actual == Some(Substitution(Var(0) -> 1337))
        )
      },
      test("Should unify list elements") {
        val actual =
          unify(List(1, 2, 3), List(Var(1), 2, Var(0)), Substitution.empty)
        assertTrue(
          actual == Some(Substitution(Var(0) -> 3, Var(1) -> 1))
        )
      }
    )
  )
}
