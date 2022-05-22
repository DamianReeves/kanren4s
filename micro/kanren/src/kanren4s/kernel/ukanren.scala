package kanren4s.kernel

object ukanren {}

trait MicroKanrenModule {
  def unify[A, B](a: A, b: B, s: Substitution): Option[Substitution]
}
