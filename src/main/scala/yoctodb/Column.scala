// Copyright (c) 2021-22 by Vadim Bondarev
// This software is licensed under the Apache License, Version 2.0.
// You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0.

package yoctodb

import izumi.reflect.Tag
import yoctodb.schema.games.v1.GamesSchema
import yoctodb.schema.games.v1.GamesSchema.*

import scala.annotation.implicitNotFound

import CEntry.*

final case class Column[+A <: CEntry[?]] private (
    private val underlying: Map[Tag[?], A],
    private val columnNames: Set[String]):
  self =>

  def columns = self.columnNames - EmptyColumn

  def narrow = self.asInstanceOf[Column[A]]

  override def toString: String = s"Schema(${columns.mkString(",")})"

//override def toString: String = s"Schema(${underlying.mkString(",")}, ${columns.mkString(",")})"

//override def hashCode(): Int = self.columns.hashCode()
/*
  // (Column(fullStage) ++ Column(awayTeam)).equals(Column(fullStage) ++ Column(awayTeam))
  override def equals(obj: Any): Boolean =
    obj.asInstanceOf[scala.Matchable] match
      case that: Column[CEntry[?]] => self.columns == that.columns
      case _                       => false
 */

/** For more information on ideas behind this implementation see:
  *
  * a) Scala3 intersection types (products, modeled as case classes)
  *
  * b) I Can Has? Exploring ZIO's Has Type. (https://youtu.be/1e0C0jUzup4?list=PLvdARMfvom9C8ss18he1P5vOcogawm5uC)
  *
  * c) DevInsideYou Part 6 - zio.Has Getting Started with #ZIO in #Scala3 (https://youtu.be/epTKGRuxbOE?t=685,
  * https://github.com/DevInsideYou/zionutshell/blob/main/src/main/scala/dev/insideyou/zionutshell/Has.scala)
  *
  * d) Scala 3: Anti-Givens https://youtu.be/HQojyuZK-Uo
  */
object Column:

  // generates because Column[GameDay] is not a supertype of Column[GameFullStage & GameAwayTeam]
  implicitly[scala.util.NotGiven[Column[GameFullStage & GameAwayTeam] <:< Column[GameDay]]]

  // Column[GameAwayTeam] is a supertype of Column[GameFullStage & GameAwayTeam]
  implicitly[Column[GameFullStage & GameAwayTeam] <:< Column[GameAwayTeam]]

  // fails to generate ev: NotGiven because Column[GameAwayTeam] is a supertype of Column[GameFullStage & GameAwayTeam]
  // implicitly[scala.util.NotGiven[Column[GameFullStage & GameAwayTeam] <:< Column[GameAwayTeam]]]

  def apply[A <: CEntry[?]](value: A)(using tag: Tag[A]): Column[A] =
    new Column(Map(tag -> value), Set(value.columnName))

  extension [Schema <: Column[?]](schema: Schema)

    /** The `ev` instance will be generated only if Column[B] is a super type of Schema.
      *
      * Examples:
      *
      * a) Column[B] is a super type of Column[A & B]
      *
      * b) Column[B] is not a super type of Column[A & C]
      */
    inline def ++[B <: CEntry[?]](
        that: Column[B]
      )(using
        @implicitNotFound("A duplicate column!")
        ev: scala.util.NotGiven[Schema <:< Column[B]]
      ): Schema & Column[B] = union(that)

    infix def union[B <: CEntry[?]](
        that: Column[B]
      )(using
        @implicitNotFound("A duplicate column!")
        ev: scala.util.NotGiven[Schema <:< Column[B]]
      ): Schema & Column[B] =
      new Column(
        (schema.underlying ++ that.underlying).asInstanceOf[Map[Tag[?], CEntry[?]]],
        schema.columnNames ++ that.columnNames,
      ).asInstanceOf[Schema & Column[B]]

    /*
      2.13.8
      def column[T <: CEntry[?]](implicit tag: Tag[T], ev: Schema <:< Column[T]): T =
        schema.underlying(tag).asInstanceOf[T]

      Or
      def column[T <: CEntry[?]](using tag: Tag[T], ev: Schema <:< Column[T]): T =
        schema.underlying(tag).asInstanceOf[T]
     */

    def column[T <: CEntry[?]](using tag: Tag[T], ev: Schema => Column[T]): T =
      schema.underlying(tag).asInstanceOf[T]

    def where(
        buildQuery: Schema => com.yandex.yoctodb.query.Where // Query
      ): com.yandex.yoctodb.query.Where = buildQuery(schema)

    def orderBy(
        buildQuery: Schema => com.yandex.yoctodb.query.Select
      ): com.yandex.yoctodb.query.Select = buildQuery(schema)
