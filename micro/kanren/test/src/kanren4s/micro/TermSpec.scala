package kanren4s.micro
import zio.test._
object TermSpec extends DefaultRunnableSpec {
  def spec = suite("Term Spec")(
    test("A Variable Term should have the propet toString value") {
      val variable = Var.zero
      val actual = Term.Variable(variable)

      assertTrue(actual.toString == "x0")
    }
  )

}
