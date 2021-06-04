// Copyright (c) 2021 by Vadim Bondarev
// This software is licensed under the Apache License, Version 2.0.
// You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0.

package object yoctodb {

  val EmptyColumn = "empty"

  sealed trait TermOps[T]

  object EmptyTermOps extends TermOps[Nothing]

  trait FilterableOps[T] extends TermOps[T] {

    def eq$(v: T): com.yandex.yoctodb.query.TermCondition

    def notEq$(v: T): com.yandex.yoctodb.query.Condition = com.yandex.yoctodb.query.QueryBuilder.not(eq$(v))

  }

  trait SetOps[T] extends TermOps[T] {

    def in$(vs: scala.collection.immutable.Set[T]): com.yandex.yoctodb.query.TermCondition
  }

  trait InEquality[T] extends FilterableOps[T] with SetOps[T]

  trait OrderingOps[T] extends TermOps[T] {

    def descOrd: com.yandex.yoctodb.query.Order

    def ascOrd: com.yandex.yoctodb.query.Order

  }

  trait NumericOps[T] extends FilterableOps[T] with SetOps[T] {

    def gt$(v: T): com.yandex.yoctodb.query.TermCondition

    def gte$(v: T): com.yandex.yoctodb.query.TermCondition

    def lt$(v: T): com.yandex.yoctodb.query.TermCondition

    def lte$(v: T): com.yandex.yoctodb.query.TermCondition

  }

  import zio.prelude._
  import zio.test.Assertion._

  object Team extends SubtypeSmart[String](matchesRegex("lal|lac|por|chi|sea|hou|mia|okc|den|mil|ind|atl|min|tor|gsw"))
  type Team = Team.Type

  object Stage extends SubtypeSmart[String](matchesRegex("(season|playoff)-[0-9]{2}-[0-9]{2}"))
  type Stage = Stage.Type

  //implicitly[String <:< CharSequence]
}
