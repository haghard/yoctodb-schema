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

object GamesIndex {
  val IndexName         = "games"
  val PayloadColumnName = "g_payload"
  val InfoColumnName    = "g_info"

  trait Games  extends Column[Games]
  object Games extends Games

  //Protoc columns
  val stage    = Index.GamesStage(games_stage(GamesSchema.FieldType.Str, GamesSchema.IndexType.Filterable))
  val awayTeam = Index.GamesAt(games_at(GamesSchema.FieldType.Str, GamesSchema.IndexType.Filterable))
  val homeTeam = Index.GamesHt(games_ht(GamesSchema.FieldType.Str, GamesSchema.IndexType.Filterable))
  val winner   = Index.GamesWinner(games_winner(GamesSchema.FieldType.Str, GamesSchema.IndexType.Filterable))
  val time     = Index.GamesTs(games_ts(GamesSchema.FieldType.Lng, GamesSchema.IndexType.Sortable))

  //Protoc filterable games schema as a value
  val filterableProtoc = Games + stage + awayTeam + homeTeam + winner

  //Protoc sortable games schema as a value
  val sortableProtoc = Games + time

  //Precisely defined schemas as a values
  val Filterable =
    SchemaColumn(FullStage()) ++ SchemaColumn(AwayTeam()) ++ SchemaColumn(HomeTeam()) ++ SchemaColumn(GameWinner())
  val Sortable = SchemaColumn(GameTime())

  //**************************************************************************************************************/
  final case class FullStage(index: GamesSchema.Index = stage) extends ColumnOps[String] {
    val term = new Inequality[String] {
      override def eq$(stageName: String)      = equal(fieldName, from(stageName))
      override def in$(stages: Vector[String]) = in(fieldName, stages.map(from(_)): _*)
    }
  }

  final case class AwayTeam(index: GamesSchema.Index = awayTeam) extends ColumnOps[String] {
    val term = new Inequality[String] {
      override def eq$(team: String)          = equal(fieldName, from(team))
      override def in$(teams: Vector[String]) = in(fieldName, teams.map(from(_)): _*)
    }
  }

  final case class HomeTeam(index: GamesSchema.Index = homeTeam) extends ColumnOps[String] {
    val term = new Inequality[String] {
      override def eq$(team: String)          = equal(fieldName, from(team))
      override def in$(teams: Vector[String]) = in(fieldName, teams.map(from(_)): _*)
    }
  }

  final case class GameWinner(index: GamesSchema.Index = winner) extends ColumnOps[String] {
    val term = new Inequality[String] {
      override def eq$(team: String)          = equal(fieldName, from(team))
      override def in$(teams: Vector[String]) = in(fieldName, teams.map(from(_)): _*)
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
  //*****************************************************************************************************************/

  //TODO: return Either
  def validate[T](exp: T, acc: Set[String]): Boolean = {
    def go[T](exp: T, acc: Set[String]): Set[String] =
      exp match {
        case Games ⇒ acc
        case a * b ⇒ go(b, go(a, acc))
        case ind: GamesSchema.Index ⇒
          ind match {
            case Index.GamesStage(v)  ⇒ acc - v.companion.scalaDescriptor.name
            case Index.GamesAt(v)     ⇒ acc - v.companion.scalaDescriptor.name
            case Index.GamesHt(v)     ⇒ acc - v.companion.scalaDescriptor.name
            case Index.GamesTs(v)     ⇒ acc - v.companion.scalaDescriptor.name
            case Index.GamesWinner(v) ⇒ acc - v.companion.scalaDescriptor.name
            case Index.Empty          ⇒ acc - EmptyName
          }
      }
    val r = go(exp, acc)
    if (r.nonEmpty) println(s"Schema [${r.mkString(",")}] are|is missed !")
    r.isEmpty
  }

  def rawFilterableSchema[T](
    exp: T,
    acc: SchemaColumn[ColumnOps[_]] = SchemaColumn(Empty(GamesSchema.Index.Empty)).asPhantom
  ): SchemaColumn[ColumnOps[_]] =
    exp match {
      case Games ⇒ acc
      case a * b ⇒ rawFilterableSchema(a, acc) ++ rawFilterableSchema(b, acc)
      case ind: GamesSchema.Index ⇒
        ind match {
          case v: Index.GamesStage ⇒
            if (v.value.indexType.isFilterable) acc ++ SchemaColumn(FullStage(v)).asPhantom else acc
          case v: Index.GamesAt ⇒
            if (v.value.indexType.isFilterable) acc ++ SchemaColumn(AwayTeam(v)).asPhantom else acc
          case v: Index.GamesHt ⇒
            if (v.value.indexType.isFilterable) acc ++ SchemaColumn(HomeTeam(v)).asPhantom else acc
          case v: Index.GamesTs ⇒
            if (v.value.indexType.isFilterable) acc ++ SchemaColumn(GameTime(v)).asPhantom else acc
          case v: Index.GamesWinner ⇒
            if (v.value.indexType.isFilterable) acc ++ SchemaColumn(GameWinner(v)).asPhantom else acc
          case Index.Empty ⇒ acc
        }
    }

  def rawSortableSchema[T](
    exp: T,
    acc: SchemaColumn[ColumnOps[_]] = SchemaColumn(Empty(GamesSchema.Index.Empty)).asPhantom
  ): SchemaColumn[ColumnOps[_]] =
    exp match {
      case Games ⇒ acc
      case a * b ⇒ rawSortableSchema(a, acc) ++ rawSortableSchema(b, acc)
      case ind: GamesSchema.Index ⇒
        ind match {
          case v: Index.GamesStage ⇒
            if (v.value.indexType.isSortable) acc ++ SchemaColumn(FullStage(v)).asPhantom else acc
          case v: Index.GamesAt ⇒
            if (v.value.indexType.isSortable) acc ++ SchemaColumn(AwayTeam(v)).asPhantom else acc
          case v: Index.GamesHt ⇒
            if (v.value.indexType.isSortable) acc ++ SchemaColumn(HomeTeam(v)).asPhantom else acc
          case v: Index.GamesTs ⇒
            if (v.value.indexType.isSortable) acc ++ SchemaColumn(GameTime(v)).asPhantom else acc
          case v: Index.GamesWinner ⇒
            if (v.value.indexType.isSortable) acc ++ SchemaColumn(GameWinner(v)).asPhantom else acc
          case Index.Empty ⇒ acc
        }
    }

  def showSchema[T](exp: T): String = {
    def indType(indexType: GamesSchema.IndexType) =
      indexType match {
        case IndexType.Filterable ⇒ "Filterable"
        case IndexType.Sortable   ⇒ "Sortable"
        //case IndexType.Full            ⇒ "Full"
        //case IndexType.Stored          ⇒ "Stored"
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
        case Index.GamesStage(v) ⇒
          "[" + v.companion.scalaDescriptor.name + ":" + fieldType(v.`type`) + ":" + indType(v.indexType) + "]"
        case Index.GamesAt(v) ⇒
          "[" + v.companion.scalaDescriptor.name + ":" + fieldType(v.`type`) + ":" + indType(v.indexType) + "]"
        case Index.GamesHt(v) ⇒
          "[" + v.companion.scalaDescriptor.name + ":" + fieldType(v.`type`) + ":" + indType(v.indexType) + "]"
        case Index.GamesTs(v) ⇒
          "[" + v.companion.scalaDescriptor.name + ":" + fieldType(v.`type`) + ":" + indType(v.indexType) + "]"
        case Index.GamesWinner(v) ⇒
          "[" + v.companion.scalaDescriptor.name + ":" + fieldType(v.`type`) + ":" + indType(v.indexType) + "]"
        case Index.Empty ⇒ EmptyName
      }

    exp match {
      case Games                  ⇒ IndexName
      case *(a, b)                ⇒ showSchema(a) + ", " + showSchema(b)
      case ind: GamesSchema.Index ⇒ parseIndexField(ind)
    }
  }

  /*
  def sortableSchemaFromProtoc(protocColumns: List[GamesSchema.Index]): SchemaColumn[IndexOps[_]] =
    protocColumns
      .foldLeft(SchemaColumn(Empty(GamesSchema.Index.Empty))) { (acc, protoCol) ⇒
        protoCol match {
          case v: GamesSchema.Index.GamesStage ⇒
            if (v.value.indexType.isSortable) acc ++ SchemaColumn(FullStage(v)) else acc
          case v: GamesSchema.Index.GamesAt ⇒
            if (v.value.indexType.isSortable) acc ++ SchemaColumn(AwayTeam(v)) else acc
          case v: GamesSchema.Index.GamesHt ⇒
            if (v.value.indexType.isSortable) acc ++ SchemaColumn(HomeTeam(v)) else acc
          case v: GamesSchema.Index.GamesTs ⇒
            if (v.value.indexType.isSortable) acc ++ SchemaColumn(GameTime(v)) else acc
          case GamesSchema.Index.Empty ⇒ acc
        }
      }
      .asInstanceOf[SchemaColumn[IndexOps[_]]]

  def filteredSchemaFromProtoc(protocColumns: List[GamesSchema.Index]): SchemaColumn[IndexOps[_]] =
    protocColumns
      .foldLeft(SchemaColumn(Empty(GamesSchema.Index.Empty))) { (acc, protoCol) ⇒
        protoCol match {
          case v: GamesSchema.Index.GamesStage ⇒
            if (v.value.indexType.isFilterable) acc ++ SchemaColumn(FullStage(v)) else acc
          case v: GamesSchema.Index.GamesAt ⇒
            if (v.value.indexType.isFilterable) acc ++ SchemaColumn(AwayTeam(v)) else acc
          case v: GamesSchema.Index.GamesHt ⇒
            if (v.value.indexType.isFilterable) acc ++ SchemaColumn(HomeTeam(v)) else acc
          case v: GamesSchema.Index.GamesTs ⇒
            if (v.value.indexType.isFilterable) acc ++ SchemaColumn(GameTime(v)) else acc
          case GamesSchema.Index.Empty ⇒ acc
        }
      }
      .asInstanceOf[SchemaColumn[IndexOps[_]]]
   */

}
