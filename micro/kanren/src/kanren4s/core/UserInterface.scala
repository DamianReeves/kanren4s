package kanren4s.core

trait UserInterface extends MicroKanrenCore {
  def exists(f: () => Goal): Goal = f()
  def exists(f: (Var) => Goal): Goal = callFresh(f)
  def exists(f: (Var, Var) => Goal): Goal = callFresh(q => exists(f(q, _)))
  def exists(f: (Var, Var, Var) => Goal): Goal =
    callFresh(q => exists(f(q, _, _)))
  def exists(f: (Var, Var, Var, Var) => Goal): Goal =
    callFresh(q => exists(f(q, _, _, _)))
  def exists(f: (Var, Var, Var, Var, Var) => Goal): Goal =
    callFresh(q => exists(f(q, _, _, _, _)))
  def exists(f: (Var, Var, Var, Var, Var, Var) => Goal): Goal =
    callFresh(q => exists(f(q, _, _, _, _, _)))
  def exists(f: (Var, Var, Var, Var, Var, Var, Var) => Goal): Goal =
    callFresh(q => exists(f(q, _, _, _, _, _, _)))
  def exists(f: (Var, Var, Var, Var, Var, Var, Var, Var) => Goal): Goal =
    callFresh(q => exists(f(q, _, _, _, _, _, _, _)))

}
