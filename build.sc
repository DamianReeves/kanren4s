import mill._, scalalib._
import Dependencies._

trait Kanren4sScalaModule extends ScalaModule {
  def scalacOptions = T.task { super.scalacOptions() ++ Seq("-Yrangepos") }
}

object micro extends Module {
  object kanren extends ScalaModule {
    def scalaVersion = "2.13.8"
    object test extends Tests with TestModule.Munit {
      def ivyDeps =
        Agg(org.scalameta.munit(), org.scalameta.munit.scalacheck)
    }
  }
}

object Dependencies {
  case object org {
    case object scalameta {
      case object munit {
        def apply() = ivy"org.scalameta::munit::$version"
        val version = "1.0.0-M4"
        lazy val scalacheck = ivy"org.scalameta::munit-scalacheck::$version"
      }
    }
  }
}
