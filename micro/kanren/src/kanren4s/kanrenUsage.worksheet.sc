import kanren4s.ukanren._

val res = callFresh(q => callFresh(u => conj(u === q, q === 42)))(emptyState)
println(res)

def fives: Goal =
  callFresh(q =>
    Goal.fromFunction(s => StateStream.suspend(() => disj(q === 5, fives)(s)))
  )

fives(emptyState).take(5)
