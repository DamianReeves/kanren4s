package kanren4s.micro

class GoalSuite extends munit.ScalaCheckSuite {
  test("Goal should pass when equivalent") {
    val t1 = Term.LValue(1)
    val t2 = Term.LValue(1)
    val goal = Goal.equiv(t1, t2)
    val actual = goal.evaluate()
    println(actual.toList)
    assertEquals(actual.toList.nonEmpty, true)
  }
}
