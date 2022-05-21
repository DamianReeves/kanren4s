package kanren4s.kernel

final case class State(substitutions: Substitutions, nextVar: Term.Variable) {
  self =>
}