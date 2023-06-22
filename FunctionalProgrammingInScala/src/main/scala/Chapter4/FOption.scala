package Chapter4

import Chapter4.FOption.{FNone, FSome}

enum FOption[+A] {
  case FSome(get: A)
  case FNone

  def map[B](f: A => B): FOption[B] = this match
    case FSome(value) => FSome(f(value))
    case _ => FNone

  def flatMap[B](f: A => FOption[B]): FOption[B] = this.map(f(_)).getOrElse(FNone)

  def getOrElse[B >: A](default: => B): B = this match
    case FSome(value) => value
    case _ => default

  def orElse[B >: A](ob: => FOption[B]): FOption[B] = this match
    case FNone => ob
    case _ => this

  def filter(f: A => Boolean): FOption[A] = this.flatMap(v => if f(v) then this else FNone)
}

object FOption {
  def apply[A](value: A): FOption[A] = FSome(value)
}


