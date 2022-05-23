package kanren4s.kernel

final case class Substitution(bindings: Map[Variable, Term]) { self =>

  def and(other: Substitution): Substitution =
    Substitution(bindings ++ other.bindings)

  /** Trys to add another substitution to this one; however, if the occurs check
    * fails, we return the original substitution.
    */
  def andAlsoGiven(variable: Variable, term: Term): Substitution =
    self.extend(variable, term).getOrElse(self)

  /** Adds a new binding withiout first doing an occurs check. */
  def assign(variable: Variable, term: Term): Substitution =
    Substitution(bindings + (variable -> term))

  /** Adds new bindings without first doing an occurs check. */
  def assignVariables(elems: (Variable, Term)*): Substitution =
    Substitution(bindings ++ elems.toMap)

  def extend(variable: Variable, term: Term): Option[Substitution] = {
    if (occursCheck(variable, term)) None
    else Some(Substitution(bindings + (variable -> term)))
  }

  def isEmpty: Boolean = bindings.isEmpty

  def occursCheck(
      variable: Variable,
      term: Term
  ): Boolean = {
    val t = walk(term)
    t match {
      case v @ Variable(_, _) => variable == v
      case (head :: tail) =>
        occursCheck(variable, head) || occursCheck(variable, tail)
      case _ => false
    }
  }

  def unify(t1: Term, t2: Term): Option[Substitution] =
    Substitution.unify(t1, t2)(self)

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
object Substitution {
  val empty: Substitution = Substitution(Map.empty)

  /** Sets up an initial set of substitutions without doing any occurs checks or
    * other validations.
    */
  def setupUnchecked(bindings: Map[Variable, Term]): Substitution =
    Substitution(bindings)

  /** Sets up an initial set of substitutions without doing any occurs checks or
    * other validations.
    */
  def setupUnchecked(bindings: (Variable, Term)*): Substitution =
    setupUnchecked(bindings.toMap)

  def unify(lhs: Term, rhs: Term)(
      s: Substitution,
      enableOccursCheck: Boolean = true
  ): Option[Substitution] = {
    val t1 = s.walk(lhs)
    val t2 = s.walk(rhs)
    if (t1 == t2) {
      // the terms are equal, no unification is needed
      Some(s)
    } else {
      (t1, t2) match {
        case (t1 @ Variable(_, _), t2) =>
          // Try and extend the substitution since we have a variable on the left
          if (enableOccursCheck) s.extend(t1, t2)
          else Some(s.assign(t1, t2))
        case (t1, t2 @ Variable(_, _)) =>
          // Try and unify by flipping since we have a variable on the right
          if (enableOccursCheck) s.extend(t2, t1)
          else Some(s.assign(t2, t1))
        case ((l1, l2), (r1, r2)) =>
          unify(l1, r1)(s, enableOccursCheck).flatMap(s =>
            unify(l2, r2)(s, enableOccursCheck)
          )
        case (head1 :: tail1, head2 :: tail2) =>
          unify(head1, head2)(s, enableOccursCheck) match {
            case Some(s) => unify(tail1, tail2)(s, enableOccursCheck)
            case None    => None
          }
        case _ =>
          // The terms are not equal and cannot be unified
          None
      }
    }
  }

  def walk(term: Term, substitution: Substitution): Term =
    substitution.walk(term)
}
