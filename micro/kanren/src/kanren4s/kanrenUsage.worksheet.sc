import kanren4s.ukanren._

val res = callFresh(q => callFresh(u => conj(u === q, q === 42)))(emptyState)
println(res)

def fives: Goal =
  callFresh(q =>
    Goal.fromFunction(s => StateStream.suspend(() => disj(q === 5, fives)(s)))
  )

fives(emptyState).take(5)

def unproductive: Goal = Goal.fromFunction { s =>
  unproductive(s)
}
//unproductive(emptyState)
//unproductive(emptyState).toLazyList

def unproductive2: Goal = Goal.fromFunction { s =>
  println("unproductive2")
  // TODO: Figure out how we can know we need to do such a transformation... do we just do it for all function invocations?
  StateStream.empty ++ StateStream.suspend(() => unproductive2(s))
}

//unproductive2(emptyState)
