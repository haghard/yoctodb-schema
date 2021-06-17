// Copyright (c) 2021 by Vadim Bondarev
// This software is licensed under the Apache License, Version 2.0.
// You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0.

package yoctodb

import com.yandex.yoctodb.query.Order
import com.yandex.yoctodb.query.QueryBuilder.asc
import com.yandex.yoctodb.query.QueryBuilder.desc
import com.yandex.yoctodb.query.QueryBuilder.gt as greaterThan
import com.yandex.yoctodb.query.QueryBuilder.gte as greaterOrEqThan
import com.yandex.yoctodb.query.QueryBuilder.in as multiEquality
import com.yandex.yoctodb.query.QueryBuilder.lt as lesserThan
import com.yandex.yoctodb.query.QueryBuilder.lte as lesserOrEqThan
import com.yandex.yoctodb.query.QueryBuilder.eq as equality
import com.yandex.yoctodb.util.UnsignedByteArrays.from
import com.yandex.yoctodb.v1.immutable.V1Database
import yoctodb.schema.games.v1.GamesSchema
import yoctodb.schema.games.v1.GamesSchema.*
import yoctodb.schema.games.v1.GamesSchema.Pcolumn

sealed trait CEntry[A]:

  def protoColumn: Pcolumn

  def term: Ops[A]

  def columnName = stringify(protoColumn)

  private def stringify(protoColumn: Pcolumn): String =
    protoColumn match
      case Pcolumn.Stage(v)    => v.companion.scalaDescriptor.name
      case Pcolumn.AwayTeam(v) => v.companion.scalaDescriptor.name
      case Pcolumn.HomeTeam(v) => v.companion.scalaDescriptor.name
      case Pcolumn.Time(v)     => v.companion.scalaDescriptor.name
      case Pcolumn.Winner(v)   => v.companion.scalaDescriptor.name
      case Pcolumn.Year(v)     => v.companion.scalaDescriptor.name
      case Pcolumn.Month(v)    => v.companion.scalaDescriptor.name
      case Pcolumn.Day(v)      => v.companion.scalaDescriptor.name
      case Pcolumn.Empty       => yoctodb.EmptyColumn
//case Index.Fake(v)     => v.companion.scalaDescriptor.name

object CEntry:

  final case class FullStage(
      val protoColumn: Pcolumn = Pcolumn.Stage(games_stage(GamesSchema.FieldType.Str, GamesSchema.IndexType.Filterable))
    ) extends CEntry[String]:
    val term: Filterable[String] = new Filterable[String] {
      def eq$(stageName: String) = equality(columnName, from(stageName))
      def in$(stages: Set[String]) = multiEquality(columnName, stages.map(from(_)).toSeq*)
    }

  final case class AwayTeam(
      val protoColumn: Pcolumn = Pcolumn.AwayTeam(games_at(GamesSchema.FieldType.Str, GamesSchema.IndexType.Filterable))
    ) extends CEntry[String]:
    val term: Filterable[String] = new Filterable[String] {
      def eq$(team: String) = equality(columnName, from(columnName))
      def in$(teams: Set[String]) = multiEquality(columnName, teams.map(from(_)).toSeq*)
    }

  final case class HomeTeam(
      val protoColumn: Pcolumn = Pcolumn.HomeTeam(games_ht(GamesSchema.FieldType.Str, GamesSchema.IndexType.Filterable))
    ) extends CEntry[String]:
    val term: Filterable[String] = new Filterable[String] {
      def eq$(team: String) = equality(columnName, from(team))
      def in$(teams: Set[String]) = multiEquality(columnName, teams.map(from(_)).toSeq*)
    }

  final case class Year(
      val protoColumn: Pcolumn = Pcolumn.Year(games_yy(GamesSchema.FieldType.Integer, GamesSchema.IndexType.Both))
    ) extends CEntry[Int]:
    val term: BothNums[Int] = new BothNums[Int] {
      def gt$(yy: Int) = greaterThan(columnName, from(yy))
      def gte$(yy: Int) = greaterOrEqThan(columnName, from(yy))
      def lt$(yy: Int) = lesserThan(columnName, from(yy))
      def lte$(yy: Int) = lesserOrEqThan(columnName, from(yy))
      def eq$(yy: Int) = equality(columnName, from(yy))
      def in$(years: Set[Int]) = multiEquality(columnName, years.map(from(_)).toSeq*)
      val descOrd: Order = desc(columnName)
      val ascOrd: Order = asc(columnName)
    }

  final case class Month(
      val protoColumn: Pcolumn = Pcolumn.Month(games_mm(GamesSchema.FieldType.Integer, GamesSchema.IndexType.Both))
    ) extends CEntry[Int]:
    val term: BothNums[Int] = new BothNums[Int] {
      def gt$(month: Int) = greaterThan(columnName, from(month))
      def gte$(month: Int) = greaterOrEqThan(columnName, from(month))
      def lt$(month: Int) = lesserThan(columnName, from(month))
      def lte$(month: Int) = lesserOrEqThan(columnName, from(month))
      def eq$(month: Int) = equality(columnName, from(month))
      def in$(months: Set[Int]) = multiEquality(columnName, months.map(from(_)).toSeq*)
      val descOrd: Order = desc(columnName)
      val ascOrd: Order = asc(columnName)
    }

  final case class Day(
      val protoColumn: Pcolumn = Pcolumn.Day(games_dd(GamesSchema.FieldType.Integer, GamesSchema.IndexType.Both))
    ) extends CEntry[Int]:
    val term: BothNums[Int] = new BothNums[Int] {
      def gt$(day: Int) = greaterThan(columnName, from(day))
      def gte$(day: Int) = greaterOrEqThan(columnName, from(day))
      def lt$(day: Int) = lesserThan(columnName, from(day))
      def lte$(day: Int) = lesserOrEqThan(columnName, from(day))
      def eq$(day: Int) = equality(columnName, from(day))
      def in$(days: Set[Int]) = multiEquality(columnName, days.map(from(_)).toSeq*)
      val descOrd: Order = desc(columnName)
      val ascOrd: Order = asc(columnName)
    }

  final case class GameWinner(
      val protoColumn: Pcolumn = Pcolumn.Winner(games_winner(GamesSchema.FieldType.Str, GamesSchema.IndexType.Filterable))
    ) extends CEntry[String]:
    val term: Filterable[String] = new Filterable[String] {
      def eq$(team: String) = equality(columnName, from(team))
      def in$(teams: Set[String]) = multiEquality(columnName, teams.map(from(_)).toSeq*)
    }

  final case class GameTime(
      val protoColumn: Pcolumn = Pcolumn.Time(games_ts(GamesSchema.FieldType.Lng, GamesSchema.IndexType.Sortable))
    ) extends CEntry[Long]:
    val term: Sortable[Long] = new Sortable[Long] {
      val descOrd = desc(columnName)
      val ascOrd = asc(columnName)
    }

  final case class Empty(val protoColumn: Pcolumn = Pcolumn.Empty) extends CEntry[Nothing]:
    val term = EmptyTermOps

/*final class class Fake(
  val protoColumn: Pcolumn = Pcolumn.Fake(games_fake(GamesSchema.FieldType.Str, GamesSchema.IndexType.Filterable)))
) extends ColumnOps[String]
  val term = new InEquality[String] {
    def eq$(team: String)       = equal(fieldName, from(team))
    def in$(teams: Set[String]) = inside(fieldName, teams.map(from(_)).toSeq: _*)
  }
}*/
