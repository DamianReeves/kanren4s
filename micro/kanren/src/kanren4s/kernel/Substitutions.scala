package kanren4s.kernel

final case class Substitutions(bindings: Map[Variable, Term]) { self =>

  def and(other: Substitutions): Substitutions =
    Substitutions(bindings ++ other.bindings)

  /** Trys to add another substitution to this one; however, if the occurs check
    * fails, we return the original substitution.
    */
  def andAlsoGiven(variable: Variable, term: Term): Substitutions =
    self.extend(variable, term).getOrElse(self)

  /** Adds a new binding withiout first doing an occurs check. */
  def assign(variable: Variable, term: Term): Substitutions =
    Substitutions(bindings + (variable -> term))

  /** Adds new bindings without first doing an occurs check. */
  def assignVariables(elems: (Variable, Term)*): Substitutions =
    Substitutions(bindings ++ elems.toMap)

  def extend(variable: Variable, term: Term): Option[Substitutions] = {
    if (occursCheck(variable, term)) None
    else Some(Substitutions(bindings + (variable -> term)))
  }

  def occursCheck(
      variable: Variable,
      term: Term
  ): Boolean = {
    val t = walk(term)
    t match {
      case v @ Variable(_, _) => variable == v
      case Term.Pair(left, right) =>
        occursCheck(variable, left) || occursCheck(variable, right)
      case _ => false
    }
  }

  def unify(t1: Term, t2: Term): Option[Substitutions] =
    Substitutions.unify(t1, t2)(self)

  def walk(term: Term): Term = {
    def loop(t: Term, result: Option[Term]): Term =
      (t, result) match {
        case (_, Some(resolved)) => resolved
        case (v @ Variable(_, _), _) =>
          bindings.get(v) match {
            case Some(t) => loop(t, None)
            case None    => t
          }
        case _ => t
      }

    loop(term, None)
  }

  /** An alias for `walk`. It walks all substitutions looking for a value and
    * returns the original term if not substitutions were found
    */
  @inline def valueOf(candidate: Term): Term = walk(candidate)
}
object Substitutions {
  val empty: Substitutions = Substitutions(Map.empty)

  /** Sets up an initial set of substitutions without doing any occurs checks or
    * other validations.
    */
  def setupUnchecked(bindings: Map[Variable, Term]): Substitutions =
    Substitutions(bindings)

  /** Sets up an initial set of substitutions without doing any occurs checks or
    * other validations.
    */
  def setupUnchecked(bindings: (Variable, Term)*): Substitutions =
    setupUnchecked(bindings.toMap)

  def unify(lhs: Term, rhs: Term)(s: Substitutions): Option[Substitutions] = {
    val t1 = s.walk(lhs)
    val t2 = s.walk(rhs)
    println(s"Trying to unify $t1 and $t2")
    if (t1 == t2) {
      // the terms are equal, no unification is needed
      println(s"Unification: $t1 ≡ $t2")
      Some(s)
    } else {
      (t1, t2) match {
        case (t1 @ Variable(_, _), t2) =>
          // Try and extend the substitution since we have a variable on the left
          s.extend(t1, t2)
        case (t1, t2 @ Variable(_, _)) =>
          // Try and unify by flipping since we have a variable on the right
          s.extend(t2, t1)
        case (Term.Pair(l1, r1), Term.Pair(l2, r2)) =>
          // Unify the left and right sides of the pair
          unify(l1, l2)(s).flatMap(s1 => unify(r1, r2)(s1))
        case _ =>
          // The terms are not equal and cannot be unified
          None
      }
    }
  }

  def walk(term: Term, substitution: Substitutions): Term =
    substitution.walk(term)
}
