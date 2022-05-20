package kanren4s.micro

class GoalSuite extends munit.ScalaCheckSuite {
  test("Evaluating 2 variables equal a value") {
    val actual = Goal.evaluate(
      Goal.callFresh(x =>
        Goal.callFresh { y =>
          val $x = Term.Variable(x)
          val $y = Term.Variable(y)
          val $42 = Term.LValue(42)
          Goal.equiv($x, $y) and Goal.equiv($x, $42)
        }
      ),
      debug = true
    )
    // println(actual.toList)
    assertEquals(actual.toList.size, 1)
  }

}
