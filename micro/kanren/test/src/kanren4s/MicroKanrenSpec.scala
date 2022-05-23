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
      },
      test("Unify (v, u): Given v ≡ 42") {
        val v = Var(0) ?? "v"
        val u = v.next ?? "u"
        val substitution = Substitution(v -> 42)
        val actual = unify(u, v, substitution)
        assertTrue(actual == Some(Substitution(u -> 42, v -> 42)))
      }
    ),
    suite("Substitution")(
      test("value of v: Given v ≡ 42") {
        val v = Var(0) ?? "v"
        val actual = valueOf(v, Substitution(v -> 42))
        assertTrue(actual == 42)
      },
      test("value of u: Given v ≡ 42 and u ≡ v") {
        val v = Var(0) ?? "v"
        val u = v.next ?? "u"
        val actual = valueOf(u, Substitution(v -> 42, u -> v))
        assertTrue(actual == 42)
      },
      test("Multi-variable test") {
        val a = Var(0) ?? "a"
        val b = Var(1) ?? "b"
        val c = Var(2) ?? "c"
        val d = Var(3) ?? "d"
        val e = Var(4) ?? "e"
        val cat = "cat"

        val subst = Substitution(
          c -> d,
          b -> cat,
          a -> b
        )

        assertTrue(
          walk(a, subst) == cat,
          valueOf(a, subst) == walk(a, subst),
          walk(b, subst) == cat,
          valueOf(b, subst) == walk(b, subst),
          walk(c, subst) == d,
          valueOf(c, subst) == walk(c, subst),
          // Walk doesn't know anything extra about d (even though it would unify with c)
          walk(d, subst) == d,
          valueOf(d, subst) == walk(d, subst),
          // Walk knows nothing extra about e (we have no substitutions)
          walk(e, subst) == e,
          valueOf(e, subst) == walk(e, subst)
        )
      }
    )
  )
}
