import kanren4s.ukanren._

val res = callFresh(q => callFresh(u => conj(u === q, q === 42)))(emptyState)
println(res)

def fives: Goal =
  callFresh(q => Goal.fromFunction(s => disj(q === 5, Goal.snooze(fives))(s)))

def sixes: Goal =
  callFresh(q => Goal.fromFunction(s => disj(q === 6, Goal.snooze(fives))(s)))

def fivesOrSixes = disj(fives, sixes)

fivesOrSixes(emptyState).take(2)

fives(emptyState).take(10)

def unproductive: Goal = Goal.fromFunction { s =>
  disj(Goal.snooze(unproductive), Goal.succeed)(s)
}
unproductive(emptyState)
unproductive(emptyState).take(1)
//unproductive(emptyState).toLazyList

def unproductive2: Goal = Goal.fromFunction { s =>
  println("unproductive2")
  // TODO: Figure out how we can know we need to do such a transformation... do we just do it for all function invocations?
  StateStream.empty ++ StateStream.suspend(() => unproductive2(s))
}

//unproductive2(emptyState)
