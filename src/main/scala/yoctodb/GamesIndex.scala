// Copyright (c) 2021 by Vadim Bondarev
// This software is licensed under the Apache License, Version 2.0.
// You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0.

package yoctodb

import com.yandex.yoctodb.query.Order
import com.yandex.yoctodb.query.QueryBuilder.asc
import com.yandex.yoctodb.query.QueryBuilder.desc
import com.yandex.yoctodb.query.QueryBuilder.gt as greaterThan
import com.yandex.yoctodb.query.QueryBuilder.gte as greateOrEqThan
import com.yandex.yoctodb.query.QueryBuilder.in as included
import com.yandex.yoctodb.query.QueryBuilder.lt as lesserThan
import com.yandex.yoctodb.query.QueryBuilder.lte as lesserOrEqThan
import com.yandex.yoctodb.query.QueryBuilder.eq as equal
import com.yandex.yoctodb.util.UnsignedByteArrays.from
import com.yandex.yoctodb.v1.immutable.V1Database
import yoctodb.schema.games.v1.GamesSchema
import yoctodb.schema.games.v1.GamesSchema.*
import zio.prelude.Validation

object GamesIndex:

  val IndexName = "games"
  val PayloadColumnName = "g_payload"
  val InfoColumnName = "g_info"

  //private val fake     = Index.Fake(games_fake(GamesSchema.FieldType.Str, GamesSchema.IndexType.Filterable))
  //private val IndexColumns: Set[GamesSchema.Index] = Set(stage, homeTeam, awayTeam, winner, year, month, day, time /*, fake*/ )

  val fullStage =
    FullStage(Index.Stage(games_stage(GamesSchema.FieldType.Str, GamesSchema.IndexType.Filterable)))

  val awayTeam =
    AwayTeam(Index.AwayTeam(games_at(GamesSchema.FieldType.Str, GamesSchema.IndexType.Filterable)))

  val homeTeam =
    HomeTeam(Index.HomeTeam(games_ht(GamesSchema.FieldType.Str, GamesSchema.IndexType.Filterable)))

  val year = Year(Index.Year(games_yy(GamesSchema.FieldType.Integer, GamesSchema.IndexType.Both)))

  val month =
    Month(Index.Month(games_mm(GamesSchema.FieldType.Integer, GamesSchema.IndexType.Both)))

  val day =
    Day(Index.Day(games_dd(GamesSchema.FieldType.Integer, GamesSchema.IndexType.Both)))

  val time =
    GameTime(Index.Time(games_ts(GamesSchema.FieldType.Lng, GamesSchema.IndexType.Sortable)))

  val winner =
    GameWinner(
      Index.Winner(games_winner(GamesSchema.FieldType.Str, GamesSchema.IndexType.Filterable))
    )

  //private val fake     = Fake(Index.Fake(games_fake(GamesSchema.FieldType.Str, GamesSchema.IndexType.Filterable)))
  val IndexColumns: Set[GamesSchema.Index] =
    Set(
      fullStage.index,
      awayTeam.index,
      homeTeam.index,
      year.index,
      month.index,
      day.index,
      winner.index,
      time.index,
    )

  //Precisely defined filterable schema as a value
  val Filterable =
    Column(fullStage) ++ Column(awayTeam) ++ Column(homeTeam) ++ Column(
      winner
    ) ++ Column(
      year
    ) ++ Column(month) ++ Column(day) //++ Column(fake)

  //Sortable schema as a value
  val Sortable = Column(time) ++ Column(year) ++ Column(month) ++ Column(day)

  //**************************************************************************************************************/
  final case class FullStage(index: GamesSchema.Index) extends ColumnOps[String]:
    val term: InEquality[String] = new InEquality[String] {
      override def eq$(stageName: String) = equal(fieldName, from(stageName))
      override def in$(stages: Set[String]) = included(fieldName, stages.map(from(_)).toSeq*)
    }

  final case class AwayTeam(index: GamesSchema.Index) extends ColumnOps[String]:
    val term: InEquality[String] = new InEquality[String] {
      override def eq$(team: String) = equal(fieldName, from(fieldName))
      override def in$(teams: Set[String]) = included(fieldName, teams.map(from(_)).toSeq*)
    }

  final case class HomeTeam(val index: GamesSchema.Index) extends ColumnOps[String]:
    val term: InEquality[String] = new InEquality[String] {
      def eq$(team: String) = equal(fieldName, from(team))
      def in$(teams: Set[String]) = included(fieldName, teams.map(from(_)).toSeq*)
    }

  final case class Year(val index: GamesSchema.Index) extends ColumnOps[Int]:
    val term: NumericOps[Int] & OrderingOps[Int] = new NumericOps[Int] with OrderingOps[Int] {
      def gt$(yy: Int) = greaterThan(fieldName, from(yy))
      def gte$(yy: Int) = greateOrEqThan(fieldName, from(yy))
      def lt$(yy: Int) = lesserThan(fieldName, from(yy))
      def lte$(yy: Int) = lesserOrEqThan(fieldName, from(yy))
      def eq$(yy: Int) = equal(fieldName, from(yy))
      def in$(years: Set[Int]) = included(fieldName, years.map(from(_)).toSeq*)
      def descOrd: Order = desc(fieldName)
      def ascOrd: Order = asc(fieldName)
    }

  final case class Month(val index: GamesSchema.Index) extends ColumnOps[Int]:
    val term: NumericOps[Int] & OrderingOps[Int] = new NumericOps[Int] with OrderingOps[Int] {
      def gt$(month: Int) = greaterThan(fieldName, from(month))
      def gte$(month: Int) = greateOrEqThan(fieldName, from(month))
      def lt$(month: Int) = lesserThan(fieldName, from(month))
      def lte$(month: Int) = lesserOrEqThan(fieldName, from(month))
      def eq$(month: Int) = equal(fieldName, from(month))
      def in$(months: Set[Int]) = included(fieldName, months.map(from(_)).toSeq*)
      def descOrd: Order = desc(fieldName)
      def ascOrd: Order = asc(fieldName)
    }

  final case class Day(index: GamesSchema.Index) extends ColumnOps[Int]:
    val term: NumericOps[Int] & OrderingOps[Int] = new NumericOps[Int] with OrderingOps[Int] {
      def gt$(day: Int) = greaterThan(fieldName, from(day))
      def gte$(day: Int) = greateOrEqThan(fieldName, from(day))
      def lt$(day: Int) = lesserThan(fieldName, from(day))
      def lte$(day: Int) = lesserOrEqThan(fieldName, from(day))
      def eq$(day: Int) = equal(fieldName, from(day))
      def in$(days: Set[Int]) = included(fieldName, days.map(from(_)).toSeq*)
      def descOrd: Order = desc(fieldName)
      def ascOrd: Order = asc(fieldName)
    }

  final case class GameWinner(index: GamesSchema.Index) extends ColumnOps[String]:
    val term: InEquality[String] = new InEquality[String] {
      def eq$(team: String) = equal(fieldName, from(team))
      def in$(teams: Set[String]) = included(fieldName, teams.map(from(_)).toSeq*)
    }

  final case class GameTime(index: GamesSchema.Index) extends ColumnOps[Long]:
    val term: OrderingOps[Long] = new OrderingOps[Long] {
      val descOrd = desc(fieldName)
      val ascOrd = asc(fieldName)
    }

  final case class Empty(index: GamesSchema.Index = Index.Empty) extends ColumnOps[Nothing]:
    val term = EmptyTermOps

  /*final class Fake(index: GamesSchema.Index)) extends ColumnOps[String] {
    val term = new InEquality[String] {
      def eq$(team: String)       = equal(name, from(team))
      def in$(teams: Set[String]) = in(name, teams.map(from(_)).toSeq: _*)
    }
  }*/

  def checkFilteredSegment(db: V1Database, columns: Set[String]): Boolean =
    columns.forall(column => db.getFilter(column).ne(null))

  def checkSortedSegment(db: V1Database, columns: Set[String]): Boolean =
    columns.forall(column => db.getSorter(column).ne(null))

  /** In order to declare this index as "safe to use" all fields from `columnsFromSchema` should be
    * presented in `columnsFromIndex`
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
                case Index.Stage(v)    => v.companion.scalaDescriptor.name == name
                case Index.AwayTeam(v) => v.companion.scalaDescriptor.name == name
                case Index.HomeTeam(v) => v.companion.scalaDescriptor.name == name
                case Index.Time(v)     => v.companion.scalaDescriptor.name == name
                case Index.Winner(v)   => v.companion.scalaDescriptor.name == name
                case Index.Year(v)     => v.companion.scalaDescriptor.name == name
                case Index.Month(v)    => v.companion.scalaDescriptor.name == name
                case Index.Day(v)      => v.companion.scalaDescriptor.name == name
                case Index.Empty       => false
            //case Index.Fake(v)     ⇒ v.companion.scalaDescriptor.name == name
            }
            .map {
              case Index.Stage(v) =>
                "[" + v.companion.scalaDescriptor.name + ":" + fieldType(v.`type`) + ":" + indType(
                  v.indexType
                ) + "]"
              case Index.AwayTeam(v) =>
                "[" + v.companion.scalaDescriptor.name + ":" + fieldType(v.`type`) + ":" + indType(
                  v.indexType
                ) + "]"
              case Index.HomeTeam(v) =>
                "[" + v.companion.scalaDescriptor.name + ":" + fieldType(v.`type`) + ":" + indType(
                  v.indexType
                ) + "]"
              case Index.Time(v) =>
                "[" + v.companion.scalaDescriptor.name + ":" + fieldType(v.`type`) + ":" + indType(
                  v.indexType
                ) + "]"
              case Index.Winner(v) =>
                "[" + v.companion.scalaDescriptor.name + ":" + fieldType(v.`type`) + ":" + indType(
                  v.indexType
                ) + "]"
              case Index.Year(v) =>
                "[" + v.companion.scalaDescriptor.name + ":" + fieldType(v.`type`) + ":" + indType(
                  v.indexType
                ) + "]"
              case Index.Month(v) =>
                "[" + v.companion.scalaDescriptor.name + ":" + fieldType(v.`type`) + ":" + indType(
                  v.indexType
                ) + "]"
              case Index.Day(v) =>
                "[" + v.companion.scalaDescriptor.name + ":" + fieldType(v.`type`) + ":" + indType(
                  v.indexType
                ) + "]"
              case Index.Empty => ""
              //case Index.Fake(v) ⇒ "[" + v.companion.scalaDescriptor.name + ":" + fieldType(v.`type`) + ":" + indType(v.indexType) + "]"
            }
        }
        .flatten
        .mkString("\n")

  def stage(v: String): Validation[String, Stage] = Stage.make(v)

  def team(v: String): Validation[String, Team] = Team.make(v)
