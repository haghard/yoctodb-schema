// Copyright (c) 2021 by Vadim Bondarev
// This software is licensed under the Apache License, Version 2.0.
// You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0.

package yoctodb

import izumi.reflect.Tag
import yoctodb.schema.games.v1.GamesSchema
import yoctodb.schema.games.v1.GamesSchema._

final case class SchemaColumn[+A <: ColumnOps[_]] private (
  private val underlying: Map[Tag[_], A],
  columnNames: Set[String]
) { self ⇒
  def asPhantom = self.asInstanceOf[SchemaColumn[A]]
}

object SchemaColumn {

  def apply[A <: ColumnOps[_]](value: A)(implicit tag: Tag[A]): SchemaColumn[A] =
    new SchemaColumn(Map(tag → value), Set(value.fieldName))

  /** SchemaColumn[A] <: SchemaColumn[_]
    * SchemaColumn[A] with SchemaColumn[B] <: SchemaColumn[_]
    * SchemaField[A] with SchemaField[B] <: SchemaField[B]
    */
  implicit class SchemaFieldSyntax[IndexSchema <: SchemaColumn[_]](val schema: IndexSchema) extends AnyVal {

    def ++[B <: ColumnOps[_]](that: SchemaColumn[B]): IndexSchema with SchemaColumn[B] =
      SchemaColumn(
        (schema.underlying ++ that.underlying).asInstanceOf[Map[Tag[_], ColumnOps[_]]],
        schema.columnNames ++ that.columnNames
      ).asInstanceOf[IndexSchema with SchemaColumn[B]]

    def column[T <: ColumnOps[_]](implicit ev: IndexSchema <:< SchemaColumn[T], tag: Tag[T]): T =
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

trait ConditionOps[A] {
  def ops: TermOps[A]
}

trait ColumnOps[A] {
  def index: GamesSchema.Index

  def fieldName: String = index match {
    case Index.GamesStage(v)  ⇒ v.companion.scalaDescriptor.name
    case Index.GamesAt(v)     ⇒ v.companion.scalaDescriptor.name
    case Index.GamesHt(v)     ⇒ v.companion.scalaDescriptor.name
    case Index.GamesTs(v)     ⇒ v.companion.scalaDescriptor.name
    case Index.GamesWinner(v) ⇒ v.companion.scalaDescriptor.name
    case Index.Empty          ⇒ EmptyName
  }

  def term: TermOps[A]
}
