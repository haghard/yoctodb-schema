// Copyright (c) 2021 by Vadim Bondarev
// This software is licensed under the Apache License, Version 2.0.
// You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0.

package yoctodb

import com.yandex.yoctodb.query.Order
import com.yandex.yoctodb.query.QueryBuilder.asc
import com.yandex.yoctodb.query.QueryBuilder.desc
import com.yandex.yoctodb.query.QueryBuilder.gt
import com.yandex.yoctodb.query.QueryBuilder.gte
import com.yandex.yoctodb.query.QueryBuilder.in
import com.yandex.yoctodb.query.QueryBuilder.lt
import com.yandex.yoctodb.query.QueryBuilder.lte
import com.yandex.yoctodb.query.QueryBuilder.{eq ⇒ equal}
import com.yandex.yoctodb.util.UnsignedByteArrays.from
import com.yandex.yoctodb.v1.immutable.V1Database
import yoctodb.schema.games.v1.GamesSchema
import yoctodb.schema.games.v1.GamesSchema._

object GamesIndex {
  val IndexName         = "games"
  val PayloadColumnName = "g_payload"
  val InfoColumnName    = "g_info"

  private val stage                                = Index.Stage(games_stage(GamesSchema.FieldType.Str, GamesSchema.IndexType.Filterable))
  private val awayTeam                             = Index.AwayTeam(games_at(GamesSchema.FieldType.Str, GamesSchema.IndexType.Filterable))
  private val homeTeam                             = Index.HomeTeam(games_ht(GamesSchema.FieldType.Str, GamesSchema.IndexType.Filterable))
  private val winner                               = Index.Winner(games_winner(GamesSchema.FieldType.Str, GamesSchema.IndexType.Filterable))
  private val year                                 = Index.Year(games_yy(GamesSchema.FieldType.Integer, GamesSchema.IndexType.Both))
  private val month                                = Index.Month(games_mm(GamesSchema.FieldType.Integer, GamesSchema.IndexType.Both))
  private val day                                  = Index.Day(games_dd(GamesSchema.FieldType.Integer, GamesSchema.IndexType.Both))
  private val time                                 = Index.Time(games_ts(GamesSchema.FieldType.Lng, GamesSchema.IndexType.Sortable))
  private val IndexColumns: Set[GamesSchema.Index] = Set(stage, homeTeam, awayTeam, winner, year, month, day, time)

  def stage(v: String) = Stage.make(v)

  def team(v: String) = Team.make(v)

  //Precisely defined filterable schema as a value
  val Filterable =
    Column(FullStage()) ++ Column(AwayTeam()) ++ Column(HomeTeam()) ++ Column(GameWinner()) ++ Column(Year()) ++ Column(
      Month()
    ) ++ Column(Day())

  //Sortable schema as a value
  val Sortable = Column(GameTime()) ++ Column(Year()) ++ Column(Month()) ++ Column(Day())

  //**************************************************************************************************************/
  final case class FullStage(index: GamesSchema.Index = stage) extends ColumnOps[String] {
    val term = new InEquality[String] {
      override def eq$(stageName: String)   = equal(name, from(stageName))
      override def in$(stages: Set[String]) = in(name, stages.map(from(_)).toSeq: _*)
    }
  }

  final case class AwayTeam(index: GamesSchema.Index = awayTeam) extends ColumnOps[String] {
    val term = new InEquality[String] {
      override def eq$(team: String)       = equal(name, from(team))
      override def in$(teams: Set[String]) = in(name, teams.map(from(_)).toSeq: _*)
    }
  }

  final case class HomeTeam(index: GamesSchema.Index = homeTeam) extends ColumnOps[String] {
    val term = new InEquality[String] {
      def eq$(team: String)       = equal(name, from(team))
      def in$(teams: Set[String]) = in(name, teams.map(from(_)).toSeq: _*)
    }
  }

  final case class Year(index: GamesSchema.Index = year) extends ColumnOps[Int] {
    val term = new NumericOps[Int] with OrderingOps[Int] {
      def gt$(yy: Int)         = gt(name, from(yy))
      def gte$(yy: Int)        = gte(name, from(yy))
      def lt$(yy: Int)         = lt(name, from(yy))
      def lte$(yy: Int)        = lte(name, from(yy))
      def eq$(yy: Int)         = equal(name, from(yy))
      def in$(years: Set[Int]) = in(name, years.map(from(_)).toSeq: _*)
      def descOrd: Order       = desc(name)
      def ascOrd: Order        = asc(name)
    }
  }

  final case class Month(index: GamesSchema.Index = month) extends ColumnOps[Int] {
    val term = new NumericOps[Int] with OrderingOps[Int] {
      def gt$(month: Int)       = gt(name, from(month))
      def gte$(month: Int)      = gte(name, from(month))
      def lt$(month: Int)       = lt(name, from(month))
      def lte$(month: Int)      = lte(name, from(month))
      def eq$(month: Int)       = equal(name, from(month))
      def in$(months: Set[Int]) = in(name, months.map(from(_)).toSeq: _*)
      def descOrd: Order        = desc(name)
      def ascOrd: Order         = asc(name)
    }
  }

  final case class Day(index: GamesSchema.Index = day) extends ColumnOps[Int] {
    val term = new NumericOps[Int] with OrderingOps[Int] {
      def gt$(day: Int)       = gt(name, from(day))
      def gte$(day: Int)      = gte(name, from(day))
      def lt$(day: Int)       = lt(name, from(day))
      def lte$(day: Int)      = lte(name, from(day))
      def eq$(day: Int)       = equal(name, from(day))
      def in$(days: Set[Int]) = in(name, days.map(from(_)).toSeq: _*)
      def descOrd: Order      = desc(name)
      def ascOrd: Order       = asc(name)
    }
  }

  final case class GameWinner(index: GamesSchema.Index = winner) extends ColumnOps[String] {
    val term = new InEquality[String] {
      def eq$(team: String)       = equal(name, from(team))
      def in$(teams: Set[String]) = in(name, teams.map(from(_)).toSeq: _*)
    }
  }

  final case class GameTime(index: GamesSchema.Index = time) extends ColumnOps[Long] {
    val term = new OrderingOps[Long] {
      val descOrd = desc(name)
      val ascOrd  = asc(name)
    }
  }

  final case class Empty(index: GamesSchema.Index = Index.Empty) extends ColumnOps[Nothing] {
    val term = EmptyTermOps
  }

  def checkFilteredSegment(db: V1Database, columns: Set[String]): Boolean =
    columns.forall(column ⇒ db.getFilter(column).ne(null))

  def checkSortedSegment(db: V1Database, columns: Set[String]): Boolean =
    columns.forall(column ⇒ db.getSorter(column).ne(null))

  /** In order to declare this index as "safe to use" all fields from `columnsFromSchema` should be
    * presented in `columnsFromIndex`
    */
  def checkIndexAgainstSchema(
    columnsFromIndex: Set[String],
    columnFromSchema: Set[String]
  ): Boolean = columnFromSchema.forall(columnsFromIndex.contains(_))

  def showSchema(
    columnsFromIndex: Set[String]
  ): String = {
    def indType(indexType: GamesSchema.IndexType) =
      indexType match {
        case IndexType.Filterable      ⇒ "Filterable"
        case IndexType.Sortable        ⇒ "Sortable"
        case IndexType.Both            ⇒ "Both"
        case IndexType.Unrecognized(_) ⇒ "Unrecognized"
      }

    def fieldType(fieldType: GamesSchema.FieldType): String =
      fieldType match {
        case FieldType.Str             ⇒ "Str"
        case FieldType.Integer         ⇒ "Int"
        case FieldType.Dbl             ⇒ "Double"
        case FieldType.Lng             ⇒ "Long"
        case FieldType.Bytes           ⇒ "Bts"
        case FieldType.Unrecognized(_) ⇒ "Unrecognized"
      }

    "\n" +
    columnsFromIndex
      .map { name ⇒
        IndexColumns
          .find { i ⇒
            i match {
              case Index.Stage(v)    ⇒ v.companion.scalaDescriptor.name == name
              case Index.AwayTeam(v) ⇒ v.companion.scalaDescriptor.name == name
              case Index.HomeTeam(v) ⇒ v.companion.scalaDescriptor.name == name
              case Index.Time(v)     ⇒ v.companion.scalaDescriptor.name == name
              case Index.Winner(v)   ⇒ v.companion.scalaDescriptor.name == name
              case Index.Year(v)     ⇒ v.companion.scalaDescriptor.name == name
              case Index.Month(v)    ⇒ v.companion.scalaDescriptor.name == name
              case Index.Day(v)      ⇒ v.companion.scalaDescriptor.name == name
              case Index.Empty       ⇒ false
            }
          }
          .map {
            case Index.Stage(v) ⇒
              "[" + v.companion.scalaDescriptor.name + ":" + fieldType(v.`type`) + ":" + indType(v.indexType) + "]"
            case Index.AwayTeam(v) ⇒
              "[" + v.companion.scalaDescriptor.name + ":" + fieldType(v.`type`) + ":" + indType(v.indexType) + "]"
            case Index.HomeTeam(v) ⇒
              "[" + v.companion.scalaDescriptor.name + ":" + fieldType(v.`type`) + ":" + indType(v.indexType) + "]"
            case Index.Time(v) ⇒
              "[" + v.companion.scalaDescriptor.name + ":" + fieldType(v.`type`) + ":" + indType(v.indexType) + "]"
            case Index.Winner(v) ⇒
              "[" + v.companion.scalaDescriptor.name + ":" + fieldType(v.`type`) + ":" + indType(v.indexType) + "]"
            case Index.Year(v) ⇒
              "[" + v.companion.scalaDescriptor.name + ":" + fieldType(v.`type`) + ":" + indType(v.indexType) + "]"
            case Index.Month(v) ⇒
              "[" + v.companion.scalaDescriptor.name + ":" + fieldType(v.`type`) + ":" + indType(v.indexType) + "]"
            case Index.Day(v) ⇒
              "[" + v.companion.scalaDescriptor.name + ":" + fieldType(v.`type`) + ":" + indType(v.indexType) + "]"
            case Index.Empty ⇒ ""
          }
      }
      .flatten
      .mkString("\n")
  }

}
