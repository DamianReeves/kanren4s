import mill._, scalalib._
import Dependencies._

trait Kanren4sScalaModule extends ScalaModule {
  def scalacOptions = T.task { super.scalacOptions() ++ Seq("-Yrangepos") }
}

trait Kanren4sZioTestModule extends TestModule {
  def ivyDeps = super.ivyDeps() ++ Agg(dev.zio.`zio-test-sbt`)
  def testFramework = "zio.test.sbt.ZTestFramework"
}

object micro extends Module {
  object kanren extends ScalaModule {
    def scalaVersion = "2.13.8"
    object test extends Tests with Kanren4sZioTestModule {
      def ivyDeps = Agg(dev.zio.zio, dev.zio.`zio-test`, dev.zio.`zio-test-sbt`)
    }
  }
}

object Dependencies {
  case object dev {
    case object zio {
      val version = "1.0.14"
      val zio: Dep = ivy"dev.zio::zio::${version}"
      val `zio-test` = ivy"dev.zio::zio-test::${version}"
      val `zio-test-magnolia` = ivy"dev.zio::zio-test-magnolia::${version}"
      val `zio-test-sbt` = ivy"dev.zio::zio-test-sbt::${version}"
    }
  }
  case object org {
    case object scalameta {
      private val munitVersion = "1.0.0-M4"
      val munit: mill.scalalib.Dep = ivy"org.scalameta::munit::$munitVersion"
      println(s"$munit")
      val `munit-scalacheck` =
        ivy"org.scalameta::munit-scalacheck::${munitVersion}"

    }
  }
}
