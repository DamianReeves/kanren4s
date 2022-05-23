package kanren4s.kernel
import com.softwaremill.tagging._

trait MicroKanrenModule {
  type Term = Any
  final type Substitution = Map[Var, Term]
  type State = (Substitution, VariableId)

  /** Adds a new binding without first doing an occurs check. */
  def assign(
      variable: Var,
      term: Term,
      subst: Substitution
  ): Substitution =
    subst + (variable -> term)

  /** Adds new bindings without first doing an occurs check. */
  def assignVariables(elems: (Var, Term)*)(
      subst: Substitution
  ): Substitution =
    subst ++ elems.toMap

  def equalTo[A, B](a: A, b: B): Boolean = a == b

  def extend(
      variable: Var,
      term: Term,
      subst: Substitution
  ): Option[Substitution] = {
    if (occursCheck(variable, term, subst)) None
    else Some(subst + (variable -> term))
  }

  def occursCheck(
      variable: Var,
      term: Term,
      subst: Substitution
  ): Boolean = {
    val t = walk(term, subst)
    t match {
      case v @ Var(_, _) => variable == v
      case (head :: tail) =>
        occursCheck(variable, head, subst) || occursCheck(variable, tail, subst)
      case _ => false
    }
  }

  /** An alias for `walk`. It walks all substitutions looking for a value and
    * returns the original term if not substitutions were found
    */
  @inline def valueOf(candidate: Term, subst: Substitution): Term =
    walk(candidate, subst)

  def walk(term: Term, substitution: Substitution): Term = {
    def loop(t: Term, result: Option[Term]): Term =
      (t, result) match {
        case (_, Some(resolved)) => resolved
        case (v @ Var(_, _), _) =>
          substitution.get(v) match {
            case Some(t) => loop(t, None)
            case None    => t
          }
        case _ => t
      }

    loop(term, None)
  }

  def unify[A, B](a: A, b: B, s: Substitution): Option[Substitution] = {
    val t1 = walk(a, s)
    val t2 = walk(b, s)
    if (equalTo(t1, t2)) {
      // the terms are equal, so no unification is needed
      Some(s)
    } else {

      (t1, t2) match {
        case (t1 @ Var(_, _), t2) =>
          // Try and extend the substitution since we have a variable on the left
          extend(t1, t2, s)
        case (t1, t2 @ Var(_, _)) =>
          // Try and unify by flipping since we have a variable on the right
          extend(t2, t1, s)
        case (head1 :: tail1, head2 :: tail2) =>
          unify(head1, head2, s) match {
            case Some(s) => unify(tail1, tail2, s)
            case None    => None
          }
        case _ =>
          // The terms are not equal and cannot be unified
          None
      }
    }
  }

}
