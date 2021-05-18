// Copyright (c) 2021 by Vadim Bondarev
// This software is licensed under the Apache License, Version 2.0.
// You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0.

package object yoctodb {

  val EmptyColumn = "empty"

  trait ProtocColumn[A <: ProtocColumn[A]] { self: A ⇒
    def +[B <: Any](b: B): A * b.type                                            = new *(self, b)
    def +[B <: ProtocColumn[B]](b: B)(implicit ev: B <:< ProtocColumn[B]): A * B = new *(self, b)
  }

  final case class *[A, B](a: A, b: B) extends ProtocColumn[A * B] {
    override val toString: String = s"($a + $b)"
  }

  sealed trait TermOps[T]

  object EmptyTermOps extends TermOps[Nothing]

  trait FilterableOps[T] extends TermOps[T] {

    def eq$(v: T): com.yandex.yoctodb.query.TermCondition

    def notEq$(v: T): com.yandex.yoctodb.query.Condition = com.yandex.yoctodb.query.QueryBuilder.not(eq$(v))

  }

  trait SetOps[T] extends TermOps[T] {

    def in$(vs: scala.collection.immutable.Set[T]): com.yandex.yoctodb.query.TermCondition
  }

  trait Inequality[T] extends FilterableOps[T] with SetOps[T]

  trait SortableOps[T] extends TermOps[T] {

    def descOrd: com.yandex.yoctodb.query.Order

    def ascOrd: com.yandex.yoctodb.query.Order

  }

  trait NumericOps[T] extends FilterableOps[T] with SetOps[T] {

    def gt$(v: T): com.yandex.yoctodb.query.TermCondition

    def gte$(v: T): com.yandex.yoctodb.query.TermCondition

    def lt$(v: T): com.yandex.yoctodb.query.TermCondition

    def lte$(v: T): com.yandex.yoctodb.query.TermCondition

  }
}
