package yoctodb

import yoctodb.schema.games.v1.GamesSchema
import yoctodb.schema.games.v1.GamesSchema.*
import yoctodb.schema.games.v1.GamesSchema.Pcolumn

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

//Another take
//https://gist.github.com/johnynek/1e3cbddf461bd3da9b00e2f4f126c253
object Records:

  object Record:
    // use this scope to bound who can see inside the opaque type
    opaque type Rec[A <: Tuple] = Map[Pcolumn, Ops[?]] //Replace Any

    object Rec:
      type HasKey[A <: Tuple, K] =
        A match
          case (K, t) *: ? => t
          case ? *: t      => HasKey[t, K]

      type UpdateKey[A <: Tuple, K, V] <: Tuple =
        A match
          case EmptyTuple   => (K, V) *: EmptyTuple
          case (K, ?) *: t  => (K, V) *: t
          case head *: tail => head *: UpdateKey[tail, K, V]

      val empty: Rec[EmptyTuple] = Map.empty

      extension [A <: Tuple](schema: Rec[A])
        def apply[K <: Pcolumn & Singleton](key: K): HasKey[A, K] =
          schema(key).asInstanceOf[HasKey[A, K]]

        def +[K <: Pcolumn & Singleton, V <: Ops[?]](kv: (K, V)): Rec[UpdateKey[A, K, V]] =
          schema + kv

  def strName(protoColumn: Pcolumn): String =
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

  def main(args: Array[String]) =
    import Record.*

    val stage = Pcolumn.Stage(games_stage(GamesSchema.FieldType.Str, GamesSchema.IndexType.Filterable))
    val hTeam = Pcolumn.HomeTeam(games_ht(GamesSchema.FieldType.Str, GamesSchema.IndexType.Filterable))
    val aTeam = Pcolumn.AwayTeam(games_at(GamesSchema.FieldType.Str, GamesSchema.IndexType.Filterable))
    val gameTime = Pcolumn.Time(games_ts(GamesSchema.FieldType.Lng, GamesSchema.IndexType.Sortable))

    val schema /*: Rec[(aTeam.type, FilterableChars[String]) *: (stage.type, BothNums[Int]) *: (time.type, SortableNum[Long]) *: EmptyTuple]*/ =
      Rec.empty +
        (hTeam -> new FilterableChars[String] {
          val columnName = strName(hTeam)
          def eq$(stageName: String) = equality(columnName, from(stageName))
          def in$(stages: Set[String]) = multiEquality(columnName, stages.map(from(_)).toSeq*)
        }) +
        (aTeam -> new FilterableChars[String] {
          val columnName = strName(aTeam)
          def eq$(stageName: String) = equality(columnName, from(stageName))
          def in$(stages: Set[String]) = multiEquality(columnName, stages.map(from(_)).toSeq*)
        }) +
        (stage -> new BothNums[Int] {
          val columnName = strName(stage)
          def gt$(yy: Int) = greaterThan(columnName, from(yy))
          def gte$(yy: Int) = greaterOrEqThan(columnName, from(yy))
          def lt$(yy: Int) = lesserThan(columnName, from(yy))
          def lte$(yy: Int) = lesserOrEqThan(columnName, from(yy))
          def eq$(yy: Int) = equality(columnName, from(yy))
          def in$(years: Set[Int]) = multiEquality(columnName, years.map(from(_)).toSeq*)
          val descOrd: Order = desc(columnName)
          val ascOrd: Order = asc(columnName)
        }) +
        (gameTime -> new SortableNum[Long] {
          val columnName = strName(gameTime)
          val descOrd = desc(columnName)
          val ascOrd = asc(columnName)
        })

    val stageOps: BothNums[Int] = schema(stage)
    val aTeamOps: FilterableChars[String] = schema(aTeam)
    val hTeamOps: FilterableChars[String] = schema(hTeam)
    val gTimeOps: SortableNum[Long] = schema(gameTime)

    //println(hTeamOps.eq$("111"))

  end main
