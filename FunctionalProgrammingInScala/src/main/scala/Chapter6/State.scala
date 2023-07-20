package Chapter6


opaque type State[S, +A] = S => (A, S)

object State {
  extension[S, A](underlying: State[S, A])
    def run(s: S): (A, S) = underlying(s)

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      s =>
        val (a, s1) = underlying(s)
        f(a)(s1)

    def map[B](f: A => B): State[S, B] =
      underlying.flatMap(a => unit(f(a)))

    def map2[B, C](other: State[S, B])(f: (A, B) => C): State[S, C] =
      for
        a <- underlying
        b <- other
      yield f(a, b)


    def state(s: S): S = run(s)._2
    def value(s: S): A = run(s)._1

  def apply[S, A](f: S => (A, S)): State[S, A] = f

  def unit[S, A](a: A): State[S, A] = (s: S) => (a, s)

  def sequence[S, A](states: List[State[S, A]]): State[S, List[A]] =
    states.foldRight(unit[S, List[A]](List.empty[A]))(_.map2(_)(_ :: _))

  def get[S]: State[S, S] = s => (s, s)

  def set[S](s: S): State[S, Unit] = _ => ((), s)

  def modify[S](f: S => S): State[S, Unit] =
    for
      s <- get
      _ <- set(f(s))
    yield ()

}

