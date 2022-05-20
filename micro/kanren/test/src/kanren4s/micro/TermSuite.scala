package kanren4s.micro

class TermSuite extends munit.ScalaCheckSuite {
  test("A Variable Term should have the propet toString value") {
    val variable = Variable.make()
    val actual = Term.Variable(variable)

    assertEquals(actual.toString, "x0")
  }
}
