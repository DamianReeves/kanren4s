package kanren4s.micro
import zio.test._
object GoalSpec extends DefaultRunnableSpec {
  def spec = suite("Goal Spec")(
    test("Evaluating 2 variables equal a value") {
      val actual = Goal.evaluate(
        Goal.callFresh(x =>
          Goal.callFresh { y =>
            val $x = Term.Variable(x)
            val $y = Term.Variable(y)
            val $42 = Term.LValue(42)
            Goal.equiv($x, $y) and Goal.equiv($x, $42)
          }
        )
      )
      // println(actual.toList)
      assertTrue(actual.toList.size == 1)
    }
  )

}
