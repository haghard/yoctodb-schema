// Copyright (c) 2021 by Vadim Bondarev
// This software is licensed under the Apache License, Version 2.0.
// You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0.

package yoctodb

import izumi.reflect.Tag
import yoctodb.schema.games.v1.GamesSchema
import yoctodb.schema.games.v1.GamesSchema._

final case class Column[+A <: ColumnOps[_]] private (
  private val underlying: Map[Tag[_], A],
  private val columnNames: Set[String]
) { self ⇒

  def columns = self.columnNames - EmptyColumn

  def narrow = self.asInstanceOf[Column[A]]

  override def toString: String = s"ColumnSchema(${columns.mkString(",")})"

  override def equals(obj: Any): Boolean = obj match {
    case that: Column[_] ⇒ columns.equals(that.columns)
    case _               ⇒ false
  }
}

object Column {

  def apply[A <: ColumnOps[_]](value: A)(implicit tag: Tag[A]): Column[A] =
    new Column(Map(tag → value), Set(value.fieldName))

  /** Column[A] <: Column[_]
    * Column[A] with Column[B] <: Column[_]
    * Column[A] with Column[B] <: Column[A]
    * Column[A] with Column[B] <: Column[B]
    */
  implicit class SchemaColumnSyntax[IndexSchema <: Column[_]](val schema: IndexSchema) extends AnyVal {

    def ++[B <: ColumnOps[_]](that: Column[B]): IndexSchema with Column[B] =
      Column(
        (schema.underlying ++ that.underlying).asInstanceOf[Map[Tag[_], ColumnOps[_]]],
        schema.columnNames ++ that.columnNames
      ).asInstanceOf[IndexSchema with Column[B]]

    def column[T <: ColumnOps[_]](implicit ev: IndexSchema <:< Column[T], tag: Tag[T]): T =
      schema.underlying(tag).asInstanceOf[T]

    def rawColumn[T <: ColumnOps[_]](implicit tag: Tag[T]): T =
      schema.underlying(tag).asInstanceOf[T]

    def where(
      buildQuery: IndexSchema ⇒ com.yandex.yoctodb.query.Where //Query
    ): com.yandex.yoctodb.query.Where = buildQuery(schema)

    def orderBy(
      buildQuery: IndexSchema ⇒ com.yandex.yoctodb.query.Select
    ): com.yandex.yoctodb.query.Select = buildQuery(schema)
  }
}

trait ColumnOps[A] {

  def index: GamesSchema.Index

  def fieldName: String = index match {
    case Index.Stage(v)    ⇒ v.companion.scalaDescriptor.name
    case Index.AwayTeam(v) ⇒ v.companion.scalaDescriptor.name
    case Index.HomeTeam(v) ⇒ v.companion.scalaDescriptor.name
    case Index.Time(v)     ⇒ v.companion.scalaDescriptor.name
    case Index.Winner(v)   ⇒ v.companion.scalaDescriptor.name
    case Index.Empty       ⇒ EmptyColumn
  }

  def term: TermOps[A]
}
