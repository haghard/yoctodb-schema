// Copyright (c) 2021-22 by Vadim Bondarev
// This software is licensed under the Apache License, Version 2.0.
// You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0.

package yoctodb

import yoctodb.schema.games.v1.GamesSchema
import yoctodb.schema.games.v1.GamesSchema.Pcolumn

sealed trait Ops[T]

object EmptyTermOps extends Ops[Nothing]

//com.yandex.yoctodb.mutable.DocumentBuilder.IndexOption.FILTERABLE -- the field can be used to filter documents
private trait Filterable[T] extends Ops[T]:
  def eq$(v: T): com.yandex.yoctodb.query.TermCondition
  def in$(vs: scala.collection.immutable.Set[T]): com.yandex.yoctodb.query.TermCondition
  def notEq$(v: T): com.yandex.yoctodb.query.Condition = com.yandex.yoctodb.query.QueryBuilder.not(eq$(v))

trait FilterableNum[T](using n: Numeric[T]) extends Filterable[T]:
  def gt$(v: T): com.yandex.yoctodb.query.TermCondition
  def gte$(v: T): com.yandex.yoctodb.query.TermCondition
  def lt$(v: T): com.yandex.yoctodb.query.TermCondition
  def lte$(v: T): com.yandex.yoctodb.query.TermCondition

trait FilterableChars[A](using A <:< java.lang.CharSequence) extends Filterable[A]

//com.yandex.yoctodb.mutable.DocumentBuilder.IndexOption.SORTABLE -- the field can be used to sort documents
private trait Sortable[T] extends Ops[T]:
  def descOrd: com.yandex.yoctodb.query.Order
  def ascOrd: com.yandex.yoctodb.query.Order

trait SortableNum[T](using n: Numeric[T]) extends Sortable[T]
trait SortableChars[T](using T <:< java.lang.CharSequence) extends Sortable[T]

//FILTERABLE and SORTABLE
private trait Both[T] extends Ops[T]

trait BothNums[T](using n: Numeric[T]) extends Both[T]:
  def eq$(v: T): com.yandex.yoctodb.query.TermCondition
  def notEq$(v: T): com.yandex.yoctodb.query.Condition = com.yandex.yoctodb.query.QueryBuilder.not(eq$(v))
  def in$(vs: scala.collection.immutable.Set[T]): com.yandex.yoctodb.query.TermCondition
  def descOrd: com.yandex.yoctodb.query.Order
  def ascOrd: com.yandex.yoctodb.query.Order
  def gt$(v: T): com.yandex.yoctodb.query.TermCondition
  def gte$(v: T): com.yandex.yoctodb.query.TermCondition
  def lt$(v: T): com.yandex.yoctodb.query.TermCondition
  def lte$(v: T): com.yandex.yoctodb.query.TermCondition

//FILTERABLE and SORTABLE
trait BothChars[T](using T <:< java.lang.CharSequence) extends Both[T]:
  def eq$(v: T): com.yandex.yoctodb.query.TermCondition
  def notEq$(v: T): com.yandex.yoctodb.query.Condition = com.yandex.yoctodb.query.QueryBuilder.not(eq$(v))
  def in$(vs: scala.collection.immutable.Set[T]): com.yandex.yoctodb.query.TermCondition
  def descOrd: com.yandex.yoctodb.query.Order
  def ascOrd: com.yandex.yoctodb.query.Order

sealed trait Ops2[T, A]
private trait Filterable2[A, T] extends Ops2[A, T]:
  def eq$(v: T): com.yandex.yoctodb.query.TermCondition
  def in$(vs: scala.collection.immutable.Set[T]): com.yandex.yoctodb.query.TermCondition
  def notEq$(v: T): com.yandex.yoctodb.query.Condition = com.yandex.yoctodb.query.QueryBuilder.not(eq$(v))

trait FilterableChars2[T, A](using A <:< java.lang.CharSequence) extends Filterable2[T, A]
