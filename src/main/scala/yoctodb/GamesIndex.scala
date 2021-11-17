// Copyright (c) 2021 by Vadim Bondarev
// This software is licensed under the Apache License, Version 2.0.
// You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0.

package yoctodb

import com.yandex.yoctodb.query.Order
import com.yandex.yoctodb.util.UnsignedByteArrays.from
import com.yandex.yoctodb.v1.immutable.V1Database
import yoctodb.schema.games.v1.GamesSchema
import yoctodb.schema.games.v1.GamesSchema.Pcolumn
import yoctodb.schema.games.v1.GamesSchema.*
import zio.prelude.Validation

import CEntry.*

object GamesIndex:

  val IndexName = "games"
  val PayloadColumnName = "g_payload"
  val InfoColumnName = "g_info"

  // Columns
  val fullStage = FullStage()
  val awayTeam = AwayTeam()
  val homeTeam = HomeTeam()
  val year = Year()
  val month = Month()
  val day = Day()
  val gameTime = GameTime()
  val winner = GameWinner()
  // val fake     = new Fake()

  val IndexColumns: Set[GamesSchema.Pcolumn] =
    Set(
      fullStage.protoColumn,
      awayTeam.protoColumn,
      homeTeam.protoColumn,
      year.protoColumn,
      month.protoColumn,
      day.protoColumn,
      winner.protoColumn,
      gameTime.protoColumn,
    )

  // Precisely defined filterable schema of the GamesIndex as a value
  val Filterable = Column(fullStage) ++ Column(awayTeam) ++ Column(homeTeam) ++ Column(winner) ++ Column(year)
    ++ Column(month) ++ Column(day) // ++ Fake(fake)

  // Precisely defined sortable schema of the GamesIndex
  val Sortable = Column(gameTime) ++ Column(year) ++ Column(month) ++ Column(day)

  // **************************************Proves**********************************************************************/

  type Union = Column[GameTime] | Column[Day] | Column[GameTime] // union types(sum types, modeled as sealed traits)
  type Intersection =
    Column[GameTime] & Column[Day] & Column[GameTime] // intersection types (products, modeled as case classes)

  summon[Union <:< Column[CEntry[?]]]
  // Union type is a subtype of Column[CEntry[?]] or, differently expressed: an instance of Union can be considered as being an instance of Column[CEntry[?]].

  summon[Intersection <:< Column[CEntry[?]]]

  implicitly[Column[GameTime] with Column[Day] <:< Column[CEntry[?]]]
  implicitly[Column[GameTime] <:< Column[CEntry[?]]]

  implicitly[Column[GameTime] with Column[Day] <:< Column[CEntry[?]]]

  implicitly[Column[GameTime] | Column[Day] <:< Column[CEntry[?]]]

  implicitly[Column[GameTime] with Column[Day] <:< Column[GameTime]]
  implicitly[Column[GameTime] with Column[Day] <:< Column[Day]]
  implicitly[Column[AwayTeam] <:< Column[?]]
  implicitly[Column[AwayTeam] with Column[Day] <:< Column[Day]]

  // ******************************************************************************************************************/

  def checkFilteredSegment(db: V1Database, columns: Set[String]): Boolean =
    columns.forall(column => db.getFilter(column).ne(null))

  def checkSortedSegment(db: V1Database, columns: Set[String]): Boolean =
    columns.forall(column => db.getSorter(column).ne(null))

  /** In order to declare this index as "safe to use" all fields from `columnsFromSchema` should be presented in
    * `columnsFromIndex`
    */
  def checkIndexAgainstSchema(
      columnsFromIndex: Set[String],
      columnFromSchema: Set[String],
    ): Boolean = columnFromSchema.forall(columnsFromIndex.contains(_))

  def showSchema(
      columnsFromIndex: Set[String]
    ): String =
    def indType(indexType: GamesSchema.IndexType) =
      indexType match
        case IndexType.Filterable      => "Filterable"
        case IndexType.Sortable        => "Sortable"
        case IndexType.Both            => "Both"
        case IndexType.Unrecognized(_) => "Unrecognized"

    def fieldType(fieldType: GamesSchema.FieldType): String =
      fieldType match
        case FieldType.Str             => "Str"
        case FieldType.Integer         => "Int"
        case FieldType.Dbl             => "Double"
        case FieldType.Lng             => "Long"
        case FieldType.Bytes           => "Bts"
        case FieldType.Unrecognized(_) => "Unrecognized"

    "\n" +
      columnsFromIndex
        .map { name =>
          IndexColumns
            .find { i =>
              i match
                case Pcolumn.Stage(v)    => v.companion.scalaDescriptor.name == name
                case Pcolumn.AwayTeam(v) => v.companion.scalaDescriptor.name == name
                case Pcolumn.HomeTeam(v) => v.companion.scalaDescriptor.name == name
                case Pcolumn.Time(v)     => v.companion.scalaDescriptor.name == name
                case Pcolumn.Winner(v)   => v.companion.scalaDescriptor.name == name
                case Pcolumn.Year(v)     => v.companion.scalaDescriptor.name == name
                case Pcolumn.Month(v)    => v.companion.scalaDescriptor.name == name
                case Pcolumn.Day(v)      => v.companion.scalaDescriptor.name == name
                case Pcolumn.Empty       => false
            // case Index.Fake(v)     ⇒ v.companion.scalaDescriptor.name == name
            }
            .map {
              case Pcolumn.Stage(v) =>
                "[" + v.companion.scalaDescriptor.name + ":" + fieldType(v.`type`) + ":" + indType(
                  v.indexType
                ) + "]"
              case Pcolumn.AwayTeam(v) =>
                "[" + v.companion.scalaDescriptor.name + ":" + fieldType(v.`type`) + ":" + indType(
                  v.indexType
                ) + "]"
              case Pcolumn.HomeTeam(v) =>
                "[" + v.companion.scalaDescriptor.name + ":" + fieldType(v.`type`) + ":" + indType(
                  v.indexType
                ) + "]"
              case Pcolumn.Time(v) =>
                "[" + v.companion.scalaDescriptor.name + ":" + fieldType(v.`type`) + ":" + indType(
                  v.indexType
                ) + "]"
              case Pcolumn.Winner(v) =>
                "[" + v.companion.scalaDescriptor.name + ":" + fieldType(v.`type`) + ":" + indType(
                  v.indexType
                ) + "]"
              case Pcolumn.Year(v) =>
                "[" + v.companion.scalaDescriptor.name + ":" + fieldType(v.`type`) + ":" + indType(
                  v.indexType
                ) + "]"
              case Pcolumn.Month(v) =>
                "[" + v.companion.scalaDescriptor.name + ":" + fieldType(v.`type`) + ":" + indType(
                  v.indexType
                ) + "]"
              case Pcolumn.Day(v) =>
                "[" + v.companion.scalaDescriptor.name + ":" + fieldType(v.`type`) + ":" + indType(
                  v.indexType
                ) + "]"
              case Pcolumn.Empty => ""
              // case Index.Fake(v) ⇒ "[" + v.companion.scalaDescriptor.name + ":" + fieldType(v.`type`) + ":" + indType(v.indexType) + "]"
            }
        }
        .flatten
        .mkString("\n")

  // SubtypeSmart for stage that should match defined regexp
  // See more there: thttps://youtu.be/M3HmROwOoRU?t=1357
  def stage(v: String): Validation[String, Stage] = Stage.make(v)

  // SubtypeSmart for team name that should match defined regexp
  def team(v: String): Validation[String, Team] = Team.make(v)
