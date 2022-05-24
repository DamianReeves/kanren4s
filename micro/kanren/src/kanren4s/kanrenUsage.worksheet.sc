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

def unproductive: Goal = Goal.fromFunction { s => unproductive(s) }

unproductive(emptyState)
unproductive(emptyState).take(1)
//unproductive(emptyState).toLazyList

// def unproductive2: Goal = Goal.fromFunc(s => unproductive2(s))

// unproductive2(emptyState)
// unproductive2(emptyState).take(10)
