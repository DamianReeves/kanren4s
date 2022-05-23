import kanren4s.ukanren._

val res = callFresh(q => callFresh(u => conj(u === q, q === 42)))(emptyState)
println(res)
