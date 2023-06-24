package Chapter4

import Chapter4.FEither.{FLeft, FRight}

enum FEither[+E, +A] {
  case FLeft(value: E)
  case FRight(value: A)

  def map[B](f: A => B): FEither[E, B] = this.flatMap(a => FRight(f(a)))

  def flatMap[EE >: E, B](f: A => FEither[EE, B]): FEither[EE, B] = this match
    case FRight(value) => f(value)
    case FLeft(value) => FLeft(value)

  def orElse[EE >: E, B >: A](ob: => FEither[EE, B]): FEither[EE, B] = this match
    case FLeft(_) => ob
    case _ => this


  def map2[EE >: E, B, C](that: FEither[EE, B])(f: (A, B) => C): FEither[EE, C] = for 
      a <- this
      b <- that
    yield f(a, b) 
}