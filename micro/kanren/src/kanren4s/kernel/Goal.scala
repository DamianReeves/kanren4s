package kanren4s.kernel

sealed trait Goal extends Product with Serializable { self => }
object Goal {}

sealed trait Term extends Product with Serializable { self => }
object Term {
  sealed trait Variable extends Term { self =>
    import Variable._
    def name: String
    def id: Int
    def isAnonymous: Boolean = self match {
      case Anonymous(_) => true
      case _            => false
    }

    override def toString(): String = self match {
      case Anonymous(id)   => s"#$id"
      case Named(name, id) => s"$name(#$id)"
    }
  }

  object Variable {
    private final case class Anonymous(id: Int) extends Variable {
      def name: String = s"$id"
    }

    private final case class Named(name: String, id: Int) extends Variable
  }

}
