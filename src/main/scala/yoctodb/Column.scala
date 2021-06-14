// Copyright (c) 2021 by Vadim Bondarev
// This software is licensed under the Apache License, Version 2.0.
// You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0.

package yoctodb

import izumi.reflect.Tag
import yoctodb.schema.games.v1.GamesSchema
import yoctodb.schema.games.v1.GamesSchema.*

final case class Column[+A <: ColumnOps[?]] private (
    private val underlying: Map[Tag[?], A],
    private val columnNames: Set[String],
  ) { self =>

  def columns = self.columnNames - EmptyColumn

  def narrow = self.asInstanceOf[Column[A]]

  override def toString: String = s"Schema(${columns.mkString(",")})"

  override def equals(obj: Any): Boolean = obj match
    case that: Column[?] => columns.equals(that.columns)
    case _               => false
}

object Column:

  def apply[A <: ColumnOps[?]](value: A)(using tag: Tag[A]): Column[A] =
    new Column(Map(tag -> value), Set(value.fieldName))

  extension [Schema <: Column[?]](schema: Schema)

    /** Column[A] <: Column[_] Column[A] with Column[B] <: Column[_] Column[A] with Column[B] <:
      * Column[A] Column[A] with Column[B] <: Column[B]
      *
      * Column[Time] <: Column[ColumnOps[_]] Column[Time] with Column[Day] <: Column[ColumnOps[_]]
      * Column[Time] with Column[Day] <: Column[Time] Column[Time] with Column[Day] <: Column[Day]
      *
      * implicitly[Column[GamesIndex.AwayTeam] <:< Column[_]] implicitly[Column[GamesIndex.AwayTeam]
      * with Column[GamesIndex.Day] <:< Column[GamesIndex.Day]]
      */
    def ++[B <: ColumnOps[?]](that: Column[B]): Schema & Column[B] =
      new Column(
        (schema.underlying ++ that.underlying).asInstanceOf[Map[Tag[?], ColumnOps[?]]],
        schema.columnNames ++ that.columnNames,
      ).asInstanceOf[Schema & Column[B]]

    //2.13.6
    //def column[T <: ColumnOps[?]](using ev: Schema <:< Column[T], tag: Tag[T]): T = schema.underlying(tag).asInstanceOf[T]

    def column[T <: ColumnOps[?]](using Schema => Column[T])(using tag: Tag[T]): T =
      schema.underlying(tag).asInstanceOf[T]

    def where(
        buildQuery: Schema => com.yandex.yoctodb.query.Where //Query
      ): com.yandex.yoctodb.query.Where = buildQuery(schema)

    def orderBy(
        buildQuery: Schema => com.yandex.yoctodb.query.Select
      ): com.yandex.yoctodb.query.Select = buildQuery(schema)

end Column
