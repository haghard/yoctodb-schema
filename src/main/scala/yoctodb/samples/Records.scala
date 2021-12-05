package yoctodb.samples

import com.yandex.yoctodb.query.Order
import com.yandex.yoctodb.query.QueryBuilder.asc
import com.yandex.yoctodb.query.QueryBuilder.desc
import com.yandex.yoctodb.query.QueryBuilder.eq as equality
import com.yandex.yoctodb.query.QueryBuilder.gt as greaterThan
import com.yandex.yoctodb.query.QueryBuilder.gte as greaterOrEqThan
import com.yandex.yoctodb.query.QueryBuilder.in as multiEquality
import com.yandex.yoctodb.query.QueryBuilder.lt as lesserThan
import com.yandex.yoctodb.query.QueryBuilder.lte as lesserOrEqThan
import com.yandex.yoctodb.util.UnsignedByteArrays.from
import com.yandex.yoctodb.v1.immutable.V1Database
import yoctodb.Ops
import yoctodb.*
import yoctodb.schema.games.v1.GamesSchema
import yoctodb.schema.games.v1.GamesSchema.*

//Another take on generic records: https://gist.github.com/johnynek/1e3cbddf461bd3da9b00e2f4f126c253
object Records:

  object Record:
    // use this scope to bound who can see inside the opaque type
    opaque type Rec[A <: Tuple] = Map[Pcolumn, Ops[?]]

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

      val zero: Rec[EmptyTuple] = Map.empty

      extension [A <: Tuple](schema: Rec[A])
        def apply[K <: Pcolumn & Singleton](key: K): HasKey[A, K] =
          schema(key).asInstanceOf[HasKey[A, K]]

        def +[K <: Pcolumn & Singleton, V <: Ops[?]](kv: (K, V)): Rec[UpdateKey[A, K, V]] =
          schema + kv

  def getName(protoColumn: Pcolumn): String =
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
  end getName

  val stage = Pcolumn.Stage(games_stage(GamesSchema.FieldType.Str, GamesSchema.IndexType.Filterable))
  val aTeam = Pcolumn.AwayTeam(games_at(GamesSchema.FieldType.Str, GamesSchema.IndexType.Filterable))
  val hTeam = Pcolumn.HomeTeam(games_ht(GamesSchema.FieldType.Str, GamesSchema.IndexType.Filterable))
  val gameTime = Pcolumn.Time(games_ts(GamesSchema.FieldType.Lng, GamesSchema.IndexType.Sortable))
  val winner = Pcolumn.Winner(games_winner(GamesSchema.FieldType.Str, GamesSchema.IndexType.Filterable))
  val year = Pcolumn.Year(games_yy(GamesSchema.FieldType.Integer, GamesSchema.IndexType.Both))
  val month = Pcolumn.Month(games_mm(GamesSchema.FieldType.Integer, GamesSchema.IndexType.Both))
  val day = Pcolumn.Day(games_dd(GamesSchema.FieldType.Integer, GamesSchema.IndexType.Both))

  import Record.*

  val sortableSchema =
    Rec.zero + (gameTime -> new SortableNum[Long] {
      val columnName = getName(gameTime)
      val descOrd = desc(columnName)
      val ascOrd = asc(columnName)
    }) +
      (year -> new BothNums[Int] {
        val columnName = getName(year)
        def gt$(yy: Int) = greaterThan(columnName, from(yy))
        def gte$(yy: Int) = greaterOrEqThan(columnName, from(yy))
        def lt$(yy: Int) = lesserThan(columnName, from(yy))
        def lte$(yy: Int) = lesserOrEqThan(columnName, from(yy))
        def eq$(yy: Int) = equality(columnName, from(yy))
        def in$(years: Set[Int]) = multiEquality(columnName, years.map(from(_)).toSeq*)
        val descOrd: Order = desc(columnName)
        val ascOrd: Order = asc(columnName)
      }) +
      (month -> new BothNums[Int] {
        val columnName = getName(month)
        def gt$(month: Int) = greaterThan(columnName, from(month))
        def gte$(month: Int) = greaterOrEqThan(columnName, from(month))
        def lt$(month: Int) = lesserThan(columnName, from(month))
        def lte$(month: Int) = lesserOrEqThan(columnName, from(month))
        def eq$(month: Int) = equality(columnName, from(month))
        def in$(months: Set[Int]) = multiEquality(columnName, months.map(from(_)).toSeq*)
        val descOrd: Order = desc(columnName)
        val ascOrd: Order = asc(columnName)
      }) +
      (day -> new BothNums[Int] {
        val columnName = getName(day)
        def gt$(month: Int) = greaterThan(columnName, from(month))
        def gte$(month: Int) = greaterOrEqThan(columnName, from(month))
        def lt$(month: Int) = lesserThan(columnName, from(month))
        def lte$(month: Int) = lesserOrEqThan(columnName, from(month))
        def eq$(month: Int) = equality(columnName, from(month))
        def in$(months: Set[Int]) = multiEquality(columnName, months.map(from(_)).toSeq*)
        val descOrd: Order = desc(columnName)
        val ascOrd: Order = asc(columnName)
      })

  /*: Rec[(aTeam.type, FilterableChars[String]) *: (stage.type, BothNums[Int]) *: (time.type, SortableNum[Long]) *: EmptyTuple]*/
  val filterableSchema =
    Rec.zero +
      (hTeam -> new FilterableChars[String] {
        val columnName = getName(hTeam)
        def eq$(stageName: String) = equality(columnName, from(stageName))
        def in$(stages: Set[String]) = multiEquality(columnName, stages.map(from(_)).toSeq*)
      }) +
      (aTeam -> new FilterableChars[String] {
        val columnName = getName(aTeam)
        def eq$(stageName: String) = equality(columnName, from(stageName))
        def in$(stages: Set[String]) = multiEquality(columnName, stages.map(from(_)).toSeq*)
      }) +
      (stage -> new BothNums[Int] {
        val columnName = getName(stage)
        def gt$(yy: Int) = greaterThan(columnName, from(yy))
        def gte$(yy: Int) = greaterOrEqThan(columnName, from(yy))
        def lt$(yy: Int) = lesserThan(columnName, from(yy))
        def lte$(yy: Int) = lesserOrEqThan(columnName, from(yy))
        def eq$(yy: Int) = equality(columnName, from(yy))
        def in$(years: Set[Int]) = multiEquality(columnName, years.map(from(_)).toSeq*)
        val descOrd: Order = desc(columnName)
        val ascOrd: Order = asc(columnName)
      }) +
      (winner -> new FilterableChars[String] {
        val columnName = getName(winner)
        def eq$(team: String) = equality(columnName, from(team))
        def in$(teams: Set[String]) = multiEquality(columnName, teams.map(from(_)).toSeq*)
      })

  def main(args: Array[String]) =

    val stageTerm: BothNums[Int] = filterableSchema(stage)
    val aTeamTerm: FilterableChars[String] = filterableSchema(aTeam)
    val hTeamTerm: FilterableChars[String] = filterableSchema(hTeam)
    val gTimeTerm: SortableNum[Long] = sortableSchema(gameTime)

    println(filterableSchema)
    println("**************")
    println(stageTerm.eq$(2))

  end main
