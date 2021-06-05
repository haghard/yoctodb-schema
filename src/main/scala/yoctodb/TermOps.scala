package yoctodb

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
