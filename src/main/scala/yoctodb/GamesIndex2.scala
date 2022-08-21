package yoctodb

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
import yoctodb.EmptyTermOps
import yoctodb.Ops
import yoctodb.*
import yoctodb.CEntry.GameFullStage
import yoctodb.schema.games.v1.{ GamesSchema, TypeTag }
import yoctodb.schema.games.v1.GamesSchema.*

import scala.compiletime.constValue
import scala.compiletime.error
import scala.compiletime.ops.boolean.&&
import scala.compiletime.ops.boolean.||
import scala.reflect.ClassTag

object GamesIndex2:

  /*
  sealed trait TEntry[Tag <: Pcolumn, E]:
    def term: Ops[E]

  // synthesize new given instance based on existing ones
  given hTerm(using term0: FilterableChars[String]): TEntry[Pcolumn.HomeTeam, String] with
    def term: FilterableChars[String] = term0
    // def protoColumn: Pcolumn = Pcolumn.HomeTeam(games_ht(GamesSchema.FieldType.Str, GamesSchema.IndexType.Filterable))

  given numTerms[Tag <: Pcolumn, T](using /*n: Numeric[T],*/ term0: BothNums[T]): TEntry[Tag, T] with
    def term: BothNums[T] = term0

  given snumTerms[Tag <: Pcolumn, T](using /*n: Numeric[T],*/ numTerm: SortableNum[T]): TEntry[Tag, T] with
    def term: SortableNum[T] = numTerm
   */

  val stage =
    Pcolumn.Stage(games_stage(GamesSchema.FieldType.Str, GamesSchema.IndexType.Filterable))
  val aTeam =
    Pcolumn.AwayTeam(games_at(GamesSchema.FieldType.Str, GamesSchema.IndexType.Filterable))
  val hTeam =
    Pcolumn.HomeTeam(games_ht(GamesSchema.FieldType.Str, GamesSchema.IndexType.Filterable))
  val gameTime = Pcolumn.Time(games_ts(GamesSchema.FieldType.Lng, GamesSchema.IndexType.Sortable))
  val winner =
    Pcolumn.Winner(games_winner(GamesSchema.FieldType.Str, GamesSchema.IndexType.Filterable))
  val year = Pcolumn.Year(games_yy(GamesSchema.FieldType.Integer, GamesSchema.IndexType.Both))
  val month = Pcolumn.Month(games_mm(GamesSchema.FieldType.Integer, GamesSchema.IndexType.Both))
  val day = Pcolumn.Day(games_dd(GamesSchema.FieldType.Integer, GamesSchema.IndexType.Both))

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

  // Defines a mapping between protoc tag and ops allowed on this column
  // Match types
  type Term[C <: Pcolumn] = C match
    case Pcolumn.Stage      => FilterableChars[String]
    case Pcolumn.HomeTeam   => FilterableChars[String]
    case Pcolumn.AwayTeam   => FilterableChars[String]
    case Pcolumn.Winner     => FilterableChars[String]
    case Pcolumn.Time       => SortableNum[Long]
    case Pcolumn.Year       => BothNums[Int]
    case Pcolumn.Month      => BothNums[Int]
    case Pcolumn.Day        => BothNums[Int]
    case Pcolumn.Empty.type => EmptyTermOps.type
  // .asInstanceOf[scala.Matchable]

  def term[T <: Pcolumn](v: T): Term[T] = v match
    case c: Pcolumn.Stage =>
      new FilterableChars[String] {
        def eq$(stageName: String) = equality(getName(c), from(stageName))
        def in$(stages: Set[String]) = multiEquality(getName(c), stages.map(from(_)).toSeq*)
      }
    case c: Pcolumn.HomeTeam =>
      new FilterableChars[String] {
        def eq$(team: String) = equality(getName(c), from(getName(c)))
        def in$(teams: Set[String]) = multiEquality(getName(c), teams.map(from(_)).toSeq*)
      }
    case c: Pcolumn.AwayTeam =>
      new FilterableChars[String] {
        def eq$(team: String) = equality(getName(c), from(getName(c)))
        def in$(teams: Set[String]) = multiEquality(getName(c), teams.map(from(_)).toSeq*)
      }
    case c: Pcolumn.Winner =>
      new FilterableChars[String] {
        def eq$(team: String) = equality(getName(c), from(getName(c)))
        def in$(teams: Set[String]) = multiEquality(getName(c), teams.map(from(_)).toSeq*)
      }
    case c: Pcolumn.Time =>
      new SortableNum[Long] {
        val columnName = getName(c)
        val descOrd = desc(columnName)
        val ascOrd = asc(columnName)
      }
    case c: Pcolumn.Year =>
      new BothNums[Int] {
        def gt$(yy: Int) = greaterThan(getName(c), from(yy))
        def gte$(yy: Int) = greaterOrEqThan(getName(c), from(yy))
        def lt$(yy: Int) = lesserThan(getName(c), from(yy))
        def lte$(yy: Int) = lesserOrEqThan(getName(c), from(yy))
        def eq$(yy: Int) = equality(getName(c), from(yy))
        def in$(years: Set[Int]) = multiEquality(getName(c), years.map(from(_)).toSeq*)
        val descOrd: Order = desc(getName(c))
        val ascOrd: Order = asc(getName(c))
      }
    case c: Pcolumn.Month =>
      new BothNums[Int] {
        def gt$(mm: Int) = greaterThan(getName(c), from(mm))
        def gte$(mm: Int) = greaterOrEqThan(getName(c), from(mm))
        def lt$(mm: Int) = lesserThan(getName(c), from(mm))
        def lte$(mm: Int) = lesserOrEqThan(getName(c), from(mm))
        def eq$(mm: Int) = equality(getName(c), from(mm))
        def in$(mms: Set[Int]) = multiEquality(getName(c), mms.map(from(_)).toSeq*)
        val descOrd: Order = desc(getName(c))
        val ascOrd: Order = asc(getName(c))
      }
    case c: Pcolumn.Day =>
      new BothNums[Int] {
        def gt$(dd: Int) = greaterThan(getName(c), from(dd))
        def gte$(dd: Int) = greaterOrEqThan(getName(c), from(dd))
        def lt$(dd: Int) = lesserThan(getName(c), from(dd))
        def lte$(dd: Int) = lesserOrEqThan(getName(c), from(dd))
        def eq$(dd: Int) = equality(getName(c), from(dd))
        def in$(days: Set[Int]) = multiEquality(getName(c), days.map(from(_)).toSeq*)
        val descOrd: Order = desc(getName(c))
        val ascOrd: Order = asc(getName(c))
      }
    case _: Pcolumn.Empty.type => EmptyTermOps

  type HasKey[T <: Tuple, U] <: Boolean =
    T match
      case U *: r     => true
      case ? *: r     => HasKey[r, U]
      case EmptyTuple => false

  type Rem[T <: Tuple, U] <: Tuple = T match
    case U *: r     => Rem[r, U]
    case t *: r     => t *: Rem[r, U]
    case EmptyTuple => EmptyTuple

  type C[T <: Tuple, U] = U *: Rem[T, U]

  final case class IndexSchema[T <: Tuple](private val underlying: Map[Class[?], Pcolumn]):

    inline def +[A <: Pcolumn](value: A)(using tag: ClassTag[A]): IndexSchema[C[T, A]] =
      inline if !constValue[HasKey[T, A]] then IndexSchema(underlying.updated(tag.runtimeClass, value))
      else error("This content does not contain the requested type")

    inline def get[A](using tag: ClassTag[A]): A =
      inline if constValue[HasKey[T, A]] then underlying(tag.runtimeClass).asInstanceOf[A]
      else error("This content does not contain the requested type")

    inline def remove[A](using tag: ClassTag[A]): IndexSchema[Rem[T, A]] =
      inline if constValue[HasKey[T, A]] then IndexSchema[Rem[T, A]](underlying.removed(tag.runtimeClass))
      else error("This content does not contain the requested type")

  object IndexSchema:
    val empty = IndexSchema[EmptyTuple](Map.empty)

  sealed trait Column[T <: Pcolumn]
  final case class IndexSchema1[T <: Pcolumn](private val underlying: Map[Class[?], Pcolumn]):

    def get[U <: Pcolumn](implicit ev: T <:< Column[U], tag: ClassTag[U]): U =
      underlying(tag.runtimeClass).asInstanceOf[U]

    def +[U <: Pcolumn](u: U)(implicit tag: ClassTag[U]): IndexSchema1[T & Column[U]] =
      new IndexSchema1[T & Column[U]](underlying.updated(tag.runtimeClass, u))
  // &
  object IndexSchema1:
    val empty = new IndexSchema1[Pcolumn.Empty.type](Map.empty)

  final case class Schema2(
      homeTeam: FilterableChars[String],
      awayTeam: FilterableChars[String],
      gameTime: SortableNum[Long])

  type OpsF[C <: Pcolumn] = C match
    case Pcolumn.HomeTeam => FilterableChars[String]
    case Pcolumn.AwayTeam => FilterableChars[String]
    case Pcolumn.Time     => SortableNum[Long]

  type OpsDep = (column: Pcolumn) => OpsF[column.type]

  def termToF(schema: Schema2): OpsDep =
    case _: Pcolumn.HomeTeam => schema.homeTeam
    case _: Pcolumn.AwayTeam => schema.awayTeam
    case _: Pcolumn.Time     => schema.gameTime

  def termFromF(f: OpsDep): Schema2 =
    Schema2(f(hTeam), f(aTeam), f(gameTime))

  val schema0 = Schema2(
    new FilterableChars[String] {
      def eq$(team: String) = equality(getName(hTeam), from(getName(hTeam)))
      def in$(teams: Set[String]) = multiEquality(getName(hTeam), teams.map(from(_)).toSeq*)
    },
    new FilterableChars[String] {
      def eq$(team: String) = equality(getName(aTeam), from(getName(aTeam)))
      def in$(teams: Set[String]) = multiEquality(getName(aTeam), teams.map(from(_)).toSeq*)
    },
    new SortableNum[Long] {
      val descOrd = desc(getName(gameTime))
      val ascOrd = asc(getName(gameTime))
    },
  )

  val terms = termToF(schema0)
  terms(hTeam).eq$("sdsd")
  terms(aTeam).in$(Set("a", "b", "c"))
  terms(gameTime).ascOrd

  termFromF(terms)

  def main(args: Array[String]): Unit =

    val schema = IndexSchema.empty + hTeam + aTeam + stage

    // schema.get[Pcolumn.Month] compilation error

    term(hTeam).eq$("a")
    term(aTeam).eq$("b")
    term(schema.get[Pcolumn.Stage]).in$(Set("a", "b", "c"))

    println(term(stage).in$(Set("a", "b", "c")))

    val schema1 = IndexSchema1.empty + hTeam + aTeam + stage
    // schema.get[Pcolumn.Month] compilation error
    schema1.get[Pcolumn.HomeTeam]
    term(schema1.get[Pcolumn.Stage]).in$(Set("a", "b", "c"))

    println("")
  end main

end GamesIndex2
