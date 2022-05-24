package kanren4s.core
import com.softwaremill.tagging._
//import kanren4s.core.CommonTags.NaturalTag

trait SchemePredef {
  //type NaturalInput = Either[Any, Int] @@ NaturalTag

  def and[A,B,Result](cond1:Truthy[A], cond2:Truthy[B], result: => Result)(implicit ev1:Truthy[A], ev2:Truthy[B]):Result
}

object CommonTags {
  //trait NaturalTag
}


