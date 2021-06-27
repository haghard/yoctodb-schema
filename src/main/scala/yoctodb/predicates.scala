package yoctodb

object predicates:

  //Declaration side variance: inputs (-) output (+)
  final case class Predicate[-A](apply: A => Boolean):
    self =>

    def &&[A1 <: A](that: Predicate[A1]): Predicate[A1] =
      Predicate(a1 => self.apply(a1) && that.apply(a1))

    def ||[A1 <: A](that: Predicate[A1]): Predicate[A1] =
      Predicate(a1 => self.apply(a1) || that.apply(a1))

    def negate: Predicate[A] = Predicate(a => !self.apply(a))

  def isGreaterThan[T](n: T)(using N: scala.Numeric[T]): Predicate[T] =
    Predicate(N.gt(_, n))

  //def isGreaterThan[T](n: T)(using Ord: scala.Ordering[T]): Predicate[T] = Predicate(Ord.gt(_, n))

  def isGreaterOrEqualTo[T](n: T)(using N: scala.Numeric[T]): Predicate[T] =
    isGreaterThan(n) || isEqualTo(n)

  //def isLessThan(n: Int): Predicate[Int] = Predicate(_ < n)
  //def isLessThanOrEqualTo(n: Int): Predicate[Int] = isLessThan(n) || isEqualTo(n)

  def isEqualTo[T](n: T) = Predicate(_ == n)

/*
import predicates.*
  println(isGreaterThan(15).apply(10))
  println(isGreaterOrEqualTo(5).apply(5))
 */


