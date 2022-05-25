package kanren4s

trait StreamModule[Stream[+_]] {

  def append[State](s1: Stream[State], s2: Stream[State]): Stream[State]

  def appendMap[State](
      s: Stream[State],
      g: State => Stream[State]
  ): Stream[State]

  def car[State](stream: Stream[State]): State
  def cdr[State](stream: Stream[State]): Stream[State]
  def cons[State](head: State, tail: Stream[State]): Stream[State]

  /** An alias for `mzero`
    */
  @inline final def empty[State]: Stream[State] = mzero[State]

  def force[State](s: Stream[State]): Stream[State]
  def null_?[State](s: Stream[State]): Boolean
  def mzero[State]: Stream[State]
  def promise_?[State](s: Stream[State]): Stream[State]
  def pull[State](s: Stream[State])

  final def take[State](n: Int, s: Stream[State]): List[State] =
    if (n < 0) take(None, s) else take(Some(n), s)

  def take[State](n: Option[Int], s: Stream[State]): List[State] =
    if (null_?(s)) {
      List.empty
    } else {
      n match {
        case Some(n) if (n - 1) == 0 => List(car(s))
        case Some(n)                 => ??? // cons(car(s))(take(n - 1, cdr(s)))
        case _                       => ???
      }
    }

  final def takeAll[State](s: Stream[State]): List[State] = take(None, s)

}
