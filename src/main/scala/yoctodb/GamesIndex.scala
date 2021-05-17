// Copyright (c) 2021 by Vadim Bondarev
// This software is licensed under the Apache License, Version 2.0.
// You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0.

package yoctodb

import com.yandex.yoctodb.query.QueryBuilder.asc
import com.yandex.yoctodb.query.QueryBuilder.desc
import com.yandex.yoctodb.query.QueryBuilder.in
import com.yandex.yoctodb.query.QueryBuilder.{eq ⇒ equal}
import com.yandex.yoctodb.util.UnsignedByteArrays.from
import yoctodb.schema.games.v1.GamesSchema
import yoctodb.schema.games.v1.GamesSchema._

import scala.collection.immutable

object GamesIndex {
  val IndexName         = "games"
  val PayloadColumnName = "g_payload"
  val InfoColumnName    = "g_info"

  trait Games  extends ProtocColumn[Games]
  object Games extends Games

  //Protoc columns
  val stage    = Index.Stage(games_stage(GamesSchema.FieldType.Str, GamesSchema.IndexType.Filterable))
  val awayTeam = Index.AwayTeam(games_at(GamesSchema.FieldType.Str, GamesSchema.IndexType.Filterable))
  val homeTeam = Index.HomeTeam(games_ht(GamesSchema.FieldType.Str, GamesSchema.IndexType.Filterable))
  val winner   = Index.Winner(games_winner(GamesSchema.FieldType.Str, GamesSchema.IndexType.Filterable))
  val time     = Index.Time(games_ts(GamesSchema.FieldType.Lng, GamesSchema.IndexType.Sortable))

  //Protoc filterable games schema as a value
  val filterableProtoc = Games + stage + awayTeam + homeTeam + winner

  //Protoc sortable games schema as a value
  val sortableProtoc = Games + time

  //Precisely defined schemas as a values
  val Filterable =
    Column(FullStage()) ++ Column(AwayTeam()) ++ Column(HomeTeam()) ++ Column(GameWinner())

  val Sortable = Column(GameTime())

  //**************************************************************************************************************/
  final case class FullStage(index: GamesSchema.Index = stage) extends ColumnOps[String] {
    val term = new Inequality[String] {
      override def eq$(stageName: String)   = equal(fieldName, from(stageName))
      override def in$(stages: Set[String]) = in(fieldName, immutable.Seq.from(stages.map(from(_))): _*)
    }
  }

  final case class AwayTeam(index: GamesSchema.Index = awayTeam) extends ColumnOps[String] {
    val term = new Inequality[String] {
      override def eq$(team: String)       = equal(fieldName, from(team))
      override def in$(teams: Set[String]) = in(fieldName, immutable.Seq.from(teams.map(from(_))): _*)
    }
  }

  final case class HomeTeam(index: GamesSchema.Index = homeTeam) extends ColumnOps[String] {
    val term = new Inequality[String] {
      override def eq$(team: String)       = equal(fieldName, from(team))
      override def in$(teams: Set[String]) = in(fieldName, immutable.Seq.from(teams.map(from(_))): _*)
    }
  }

  final case class GameWinner(index: GamesSchema.Index = winner) extends ColumnOps[String] {
    val term = new Inequality[String] {
      override def eq$(team: String)       = equal(fieldName, from(team))
      override def in$(teams: Set[String]) = in(fieldName, immutable.Seq.from(teams.map(from(_))): _*)
    }
  }

  final case class GameTime(index: GamesSchema.Index = time) extends ColumnOps[Long] {
    val term = new SortableOps[Long] {
      override val descOrd = desc(fieldName)
      override val ascOrd  = asc(fieldName)
    }
  }

  final case class Empty(index: GamesSchema.Index = Index.Empty) extends ColumnOps[Nothing] {
    val term = EmptyTermOps
  }

  def validate[T](exp: T, acc: Set[String], pref: String): Option[String] = {
    def go[T](exp: T, acc: Set[String]): Set[String] =
      exp match {
        case Games ⇒ acc
        case a * b ⇒ go(b, go(a, acc))
        case ind: GamesSchema.Index ⇒
          ind match {
            case Index.Stage(v)    ⇒ acc - v.companion.scalaDescriptor.name
            case Index.AwayTeam(v) ⇒ acc - v.companion.scalaDescriptor.name
            case Index.HomeTeam(v) ⇒ acc - v.companion.scalaDescriptor.name
            case Index.Time(v)     ⇒ acc - v.companion.scalaDescriptor.name
            case Index.Winner(v)   ⇒ acc - v.companion.scalaDescriptor.name
            case Index.Empty       ⇒ acc - EmptyColumn
          }
      }
    val r = go(exp, acc)
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
            if (v.value.indexType.isFilterable) acc ++ Column(FullStage(v)).narrow else acc
          case v: Index.AwayTeam ⇒
            if (v.value.indexType.isFilterable) acc ++ Column(AwayTeam(v)).narrow else acc
          case v: Index.HomeTeam ⇒
            if (v.value.indexType.isFilterable) acc ++ Column(HomeTeam(v)).narrow else acc
          case v: Index.Time ⇒
            if (v.value.indexType.isFilterable) acc ++ Column(GameTime(v)).narrow else acc
          case v: Index.Winner ⇒
            if (v.value.indexType.isFilterable) acc ++ Column(GameWinner(v)).narrow else acc
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
            if (v.value.indexType.isSortable) acc ++ Column(FullStage(v)).narrow else acc
          case v: Index.AwayTeam ⇒
            if (v.value.indexType.isSortable) acc ++ Column(AwayTeam(v)).narrow else acc
          case v: Index.HomeTeam ⇒
            if (v.value.indexType.isSortable) acc ++ Column(HomeTeam(v)).narrow else acc
          case v: Index.Time ⇒
            if (v.value.indexType.isSortable) acc ++ Column(GameTime(v)).narrow else acc
          case v: Index.Winner ⇒
            if (v.value.indexType.isSortable) acc ++ Column(GameWinner(v)).narrow else acc
          case Index.Empty ⇒ acc
        }
    }

  def printSchema[T](exp: T): String = {
    def indType(indexType: GamesSchema.IndexType) =
      indexType match {
        case IndexType.Filterable      ⇒ "Filterable"
        case IndexType.Sortable        ⇒ "Sortable"
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
        case Index.Empty ⇒ EmptyColumn
      }

    exp match {
      case Games                  ⇒ IndexName
      case *(a, b)                ⇒ printSchema(a) + ", " + printSchema(b)
      case ind: GamesSchema.Index ⇒ parseIndexField(ind)
    }
  }
}
