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
import yoctodb.schema.games.v1.GamesSchema
import yoctodb.schema.games.v1.GamesSchema._

object GamesIndex {
  val IndexName         = "games"
  val PayloadColumnName = "g_payload"
  val InfoColumnName    = "g_info"

  trait Games  extends ProtocColumn[Games]
  object Games extends Games

  val stage    = Index.Stage(games_stage(GamesSchema.FieldType.Str, GamesSchema.IndexType.Filterable))
  val awayTeam = Index.AwayTeam(games_at(GamesSchema.FieldType.Str, GamesSchema.IndexType.Filterable))
  val homeTeam = Index.HomeTeam(games_ht(GamesSchema.FieldType.Str, GamesSchema.IndexType.Filterable))
  val winner   = Index.Winner(games_winner(GamesSchema.FieldType.Str, GamesSchema.IndexType.Filterable))
  val year     = Index.Year(games_yy(GamesSchema.FieldType.Integer, GamesSchema.IndexType.Both))
  val month    = Index.Month(games_mm(GamesSchema.FieldType.Integer, GamesSchema.IndexType.Both))
  val day      = Index.Day(games_dd(GamesSchema.FieldType.Integer, GamesSchema.IndexType.Both))
  val time     = Index.Time(games_ts(GamesSchema.FieldType.Lng, GamesSchema.IndexType.Sortable))

  //Protoc filterable games schema as a value
  val filterableProtoc = Games + stage + awayTeam + homeTeam + winner + year + month + day

  //Protoc sortable games schema as a value
  val sortableProtoc = Games + time + year + month + day

  //Precisely defined schemas as a values
  val Filterable =
    Column(FullStage()) ++ Column(AwayTeam()) ++ Column(HomeTeam()) ++ Column(GameWinner()) ++ Column(Year()) ++ Column(
      Month()
    ) ++ Column(Day())

  val Sortable = Column(GameTime()) ++ Column(Year()) ++ Column(Month()) ++ Column(Day())

  //**************************************************************************************************************/
  final case class FullStage(index: GamesSchema.Index = stage) extends ColumnOps[String] {
    val term = new Inequality[String] {
      override def eq$(stageName: String)   = equal(fieldName, from(stageName))
      override def in$(stages: Set[String]) = in(fieldName, stages.map(from(_)).toSeq: _*)
    }
  }

  final case class AwayTeam(index: GamesSchema.Index = awayTeam) extends ColumnOps[String] {
    val term = new Inequality[String] {
      override def eq$(team: String)       = equal(fieldName, from(team))
      override def in$(teams: Set[String]) = in(fieldName, teams.map(from(_)).toSeq: _*)
    }
  }

  final case class HomeTeam(index: GamesSchema.Index = homeTeam) extends ColumnOps[String] {
    val term = new Inequality[String] {
      def eq$(team: String)       = equal(fieldName, from(team))
      def in$(teams: Set[String]) = in(fieldName, teams.map(from(_)).toSeq: _*)
    }
  }

  final case class Year(index: GamesSchema.Index = year) extends ColumnOps[Int] {
    val term = new NumericOps[Int] with SortableOps[Int] {
      def gt$(yy: Int)         = gt(fieldName, from(yy))
      def gte$(yy: Int)        = gte(fieldName, from(yy))
      def lt$(yy: Int)         = lt(fieldName, from(yy))
      def lte$(yy: Int)        = lte(fieldName, from(yy))
      def eq$(yy: Int)         = equal(fieldName, from(yy))
      def in$(years: Set[Int]) = in(fieldName, years.map(from(_)).toSeq: _*)
      def descOrd: Order       = desc(fieldName)
      def ascOrd: Order        = asc(fieldName)
    }
  }

  final case class Month(index: GamesSchema.Index = month) extends ColumnOps[Int] {
    val term = new NumericOps[Int] with SortableOps[Int] {
      def gt$(month: Int)       = gt(fieldName, from(month))
      def gte$(month: Int)      = gte(fieldName, from(month))
      def lt$(month: Int)       = lt(fieldName, from(month))
      def lte$(month: Int)      = lte(fieldName, from(month))
      def eq$(month: Int)       = equal(fieldName, from(month))
      def in$(months: Set[Int]) = in(fieldName, months.map(from(_)).toSeq: _*)
      def descOrd: Order        = desc(fieldName)
      def ascOrd: Order         = asc(fieldName)
    }
  }

  final case class Day(index: GamesSchema.Index = day) extends ColumnOps[Int] {
    val term = new NumericOps[Int] with SortableOps[Int] {
      def gt$(day: Int)       = gt(fieldName, from(day))
      def gte$(day: Int)      = gte(fieldName, from(day))
      def lt$(day: Int)       = lt(fieldName, from(day))
      def lte$(day: Int)      = lte(fieldName, from(day))
      def eq$(day: Int)       = equal(fieldName, from(day))
      def in$(days: Set[Int]) = in(fieldName, days.map(from(_)).toSeq: _*)
      def descOrd: Order      = desc(fieldName)
      def ascOrd: Order       = asc(fieldName)
    }
  }

  final case class GameWinner(index: GamesSchema.Index = winner) extends ColumnOps[String] {
    val term = new Inequality[String] {
      def eq$(team: String)       = equal(fieldName, from(team))
      def in$(teams: Set[String]) = in(fieldName, teams.map(from(_)).toSeq: _*)
    }
  }

  final case class GameTime(index: GamesSchema.Index = time) extends ColumnOps[Long] {
    val term = new SortableOps[Long] {
      val descOrd = desc(fieldName)
      val ascOrd  = asc(fieldName)
    }
  }

  final case class Empty(index: GamesSchema.Index = Index.Empty) extends ColumnOps[Nothing] {
    val term = EmptyTermOps
  }

  def validate[T](exp: T, columnNames: Set[String], pref: String): Option[String] = {
    def parse(ind: GamesSchema.Index, acc: Set[String]): Set[String] = ind match {
      case Index.Stage(v)    ⇒ acc - v.companion.scalaDescriptor.name
      case Index.AwayTeam(v) ⇒ acc - v.companion.scalaDescriptor.name
      case Index.HomeTeam(v) ⇒ acc - v.companion.scalaDescriptor.name
      case Index.Time(v)     ⇒ acc - v.companion.scalaDescriptor.name
      case Index.Winner(v)   ⇒ acc - v.companion.scalaDescriptor.name
      case Index.Year(v)     ⇒ acc - v.companion.scalaDescriptor.name
      case Index.Month(v)    ⇒ acc - v.companion.scalaDescriptor.name
      case Index.Day(v)      ⇒ acc - v.companion.scalaDescriptor.name
      case Index.Empty       ⇒ acc - EmptyColumn
    }
    def go[T](exp: T, acc: Set[String]): Set[String] =
      exp match {
        case Games                  ⇒ acc
        case a * b                  ⇒ go(b, go(a, acc))
        case ind: GamesSchema.Index ⇒ parse(ind, acc)
      }
    val r = go(exp, columnNames)
    if (r.isEmpty) None else Some(s"$pref columns [${r.mkString(",")}] are missed.")
  }

  def rawFilterableSchema[T](
    exp: T,
    acc: Column[ColumnOps[_]] = Column(Empty(GamesSchema.Index.Empty)).narrow
  ): Column[ColumnOps[_]] =
    exp match {
      case Games ⇒ acc
      case a * b ⇒ rawFilterableSchema(a, acc) ++ rawFilterableSchema(b, acc)
      case ind: GamesSchema.Index ⇒
        ind match {
          case v: Index.Stage ⇒
            if (v.value.indexType.isBoth | v.value.indexType.isFilterable) acc ++ Column(FullStage(v)).narrow else acc
          case v: Index.AwayTeam ⇒
            if (v.value.indexType.isBoth | v.value.indexType.isFilterable) acc ++ Column(AwayTeam(v)).narrow else acc
          case v: Index.HomeTeam ⇒
            if (v.value.indexType.isBoth | v.value.indexType.isFilterable) acc ++ Column(HomeTeam(v)).narrow else acc
          case v: Index.Time ⇒
            if (v.value.indexType.isBoth | v.value.indexType.isFilterable) acc ++ Column(GameTime(v)).narrow else acc
          case v: Index.Winner ⇒
            if (v.value.indexType.isBoth | v.value.indexType.isFilterable) acc ++ Column(GameWinner(v)).narrow else acc
          case v: Index.Year ⇒
            if (v.value.indexType.isBoth | v.value.indexType.isFilterable) acc ++ Column(Year(v)).narrow else acc
          case v: Index.Month ⇒
            if (v.value.indexType.isBoth | v.value.indexType.isFilterable) acc ++ Column(Month(v)).narrow else acc
          case v: Index.Day ⇒
            if (v.value.indexType.isBoth | v.value.indexType.isFilterable) acc ++ Column(Day(v)).narrow else acc
          case Index.Empty ⇒ acc
        }
    }

  def rawSortableSchema[T](
    exp: T,
    acc: Column[ColumnOps[_]] = Column(Empty(GamesSchema.Index.Empty)).narrow
  ): Column[ColumnOps[_]] =
    exp match {
      case Games ⇒ acc
      case a * b ⇒ rawSortableSchema(a, acc) ++ rawSortableSchema(b, acc)
      case ind: GamesSchema.Index ⇒
        ind match {
          case v: Index.Stage ⇒
            if (v.value.indexType.isBoth | v.value.indexType.isSortable) acc ++ Column(FullStage(v)).narrow else acc
          case v: Index.AwayTeam ⇒
            if (v.value.indexType.isBoth | v.value.indexType.isSortable) acc ++ Column(AwayTeam(v)).narrow else acc
          case v: Index.HomeTeam ⇒
            if (v.value.indexType.isBoth | v.value.indexType.isSortable) acc ++ Column(HomeTeam(v)).narrow else acc
          case v: Index.Time ⇒
            if (v.value.indexType.isBoth | v.value.indexType.isSortable) acc ++ Column(GameTime(v)).narrow else acc
          case v: Index.Winner ⇒
            if (v.value.indexType.isBoth | v.value.indexType.isSortable) acc ++ Column(GameWinner(v)).narrow else acc
          case v: Index.Year ⇒
            if (v.value.indexType.isBoth | v.value.indexType.isSortable) acc ++ Column(Year(v)).narrow else acc
          case v: Index.Month ⇒
            if (v.value.indexType.isBoth | v.value.indexType.isSortable) acc ++ Column(Month(v)).narrow else acc
          case v: Index.Day ⇒
            if (v.value.indexType.isBoth | v.value.indexType.isSortable) acc ++ Column(Day(v)).narrow else acc
          case Index.Empty ⇒ acc
        }
    }

  def printSchema[T](exp: T): String = {
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

    def parseIndexField(field: GamesSchema.Index) =
      field match {
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
        case Index.Empty ⇒ EmptyColumn
      }

    exp match {
      case Games                  ⇒ IndexName
      case *(a, b)                ⇒ printSchema(a) + ", " + printSchema(b)
      case ind: GamesSchema.Index ⇒ parseIndexField(ind)
    }
  }
}
