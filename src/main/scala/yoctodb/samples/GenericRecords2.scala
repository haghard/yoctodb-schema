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
import yoctodb.EmptyTermOps
import yoctodb.Ops
import yoctodb.*
import yoctodb.schema.games.v1.GamesSchema
import yoctodb.schema.games.v1.GamesSchema.*

import scala.compiletime.constValue
import scala.compiletime.error
import scala.compiletime.ops.boolean.&&
import scala.compiletime.ops.boolean.||
import scala.reflect.ClassTag

//https://www.scala-lang.org/2021/02/26/tuples-bring-generic-programming-to-scala-3.html
//https://mhammons.hashnode.dev/metadata-types-with-scala-3
//https://dotty.epfl.ch/docs/reference/new-types/match-types.html
//Scala 3: Match Types | Rock the JVM https://www.youtube.com/watch?v=YiPQxIQZn5M&list=LL&index=6
//https://dotty.epfl.ch/docs/reference/metaprogramming/index.html
object GenericRecords2:

  /** Match types bring the `match` construct to the type level, allowing the creation of type-level functions that
    * return different types depending on the (statically known) input types.
    */
  type Combine[Left, Right] = Left match
    case Unit => Right
    case _ =>
      Right match
        case Unit => Left
        case _    => (Left, Right)

  /** EXERCISE 1
    *
    * Construct a value of the appropriate type, which is computed using the match type `Combine`.
    */
  val unitAndString: Combine[Unit, String] = "bla"
  summon[Combine[Unit, String] =:= String]

  val unitAndString1: Combine[Int, Unit] = 1
  summon[Combine[Int, Unit] =:= Int]

  val unitAndString2: Combine[String, Double] = ("bla", 1)
  summon[Combine[String, Double] =:= (String, Double)]

  type ElementType[X] = X match
    case String      => Char
    case Array[t]    => ElementType[t]
    case Iterable[t] => ElementType[t]
    case AnyVal      => X

  // Match Types
  type E[X] = X match
    case String          => Char
    case Array[t]        => t
    case IterableOnce[t] => t
    case _               => X

  type IsEmpty[S <: String] <: Boolean = S match
    case "" => true
    case _  => false

  summon[IsEmpty[""] =:= true]
  summon[IsEmpty["hi"] =:= false]

  val char: E[String] = 'c'
  val doub: E[List[Double]] = 1.0
  val num: E[Array[Double]] = 2.0
  val tupl: E[Option[(Int, Double)]] = (1, 2.0)

  summon[E[String] =:= Char]
  summon[E[Array[Int]] =:= Int]
  summon[E[List[Float]] =:= Float]
  implicitly[E[Nil.type] =:= Nothing]

  type HasKey[T <: Tuple, U] <: Boolean =
    T match
      case U *: r     => true
      case ? *: r     => HasKey[r, U]
      case EmptyTuple => false

  type Rem[T <: Tuple, U] <: Tuple = T match
    case U *: r     => Rem[r, U]
    case t *: r     => t *: Rem[r, U]
    case EmptyTuple => EmptyTuple

  type Column[T <: Tuple, U] = U *: Rem[T, U]

  // https://youtu.be/6OaW-_aFStA, // GADTs in Dotty
  /*import compiletime.ops.int.{ +, - }
  enum Vec[L <: Int, +T]:
    self =>
    case Nil extends Vec[0, Nothing]
    case NotNil[T]() extends Vec[Int, T]

    def tail: Vec[L - 1, T] = ???
    def head: T = ???

    def map[S](f: T => S): Vec[L, S] =
      this match
        case Vec.Nil => Vec.Nil
        case _       => f(self.head) :: self.tail.map(f)

    def ::[S >: T](x: S): Vec[L + 1, T] = ???

  end Vec*/

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
//.asInstanceOf[scala.Matchable]

  def column[T <: Pcolumn](v: T): Term[T] = v match
    case c: Pcolumn.Stage =>
      new FilterableChars[String]:
        def eq$(stageName: String) = equality(getName(c), from(stageName))
        def in$(stages: Set[String]) = multiEquality(getName(c), stages.map(from(_)).toSeq*)
    case c: Pcolumn.HomeTeam =>
      new FilterableChars[String]:
        def eq$(team: String) = equality(getName(c), from(getName(c)))
        def in$(teams: Set[String]) = multiEquality(getName(c), teams.map(from(_)).toSeq*)
    case c: Pcolumn.AwayTeam =>
      new FilterableChars[String]:
        def eq$(team: String) = equality(getName(c), from(getName(c)))
        def in$(teams: Set[String]) = multiEquality(getName(c), teams.map(from(_)).toSeq*)
    case c: Pcolumn.Winner =>
      new FilterableChars[String]:
        def eq$(team: String) = equality(getName(c), from(getName(c)))
        def in$(teams: Set[String]) = multiEquality(getName(c), teams.map(from(_)).toSeq*)
    case c: Pcolumn.Time =>
      new SortableNum[Long]:
        val columnName = getName(c)
        val descOrd = desc(columnName)
        val ascOrd = asc(columnName)
    case c: Pcolumn.Year =>
      new BothNums[Int]:
        def gt$(yy: Int) = greaterThan(getName(c), from(yy))
        def gte$(yy: Int) = greaterOrEqThan(getName(c), from(yy))
        def lt$(yy: Int) = lesserThan(getName(c), from(yy))
        def lte$(yy: Int) = lesserOrEqThan(getName(c), from(yy))
        def eq$(yy: Int) = equality(getName(c), from(yy))
        def in$(years: Set[Int]) = multiEquality(getName(c), years.map(from(_)).toSeq*)
        val descOrd: Order = desc(getName(c))
        val ascOrd: Order = asc(getName(c))
    case c: Pcolumn.Month =>
      new BothNums[Int]:
        def gt$(mm: Int) = greaterThan(getName(c), from(mm))
        def gte$(mm: Int) = greaterOrEqThan(getName(c), from(mm))
        def lt$(mm: Int) = lesserThan(getName(c), from(mm))
        def lte$(mm: Int) = lesserOrEqThan(getName(c), from(mm))
        def eq$(mm: Int) = equality(getName(c), from(mm))
        def in$(mms: Set[Int]) = multiEquality(getName(c), mms.map(from(_)).toSeq*)
        val descOrd: Order = desc(getName(c))
        val ascOrd: Order = asc(getName(c))
    case c: Pcolumn.Day =>
      new BothNums[Int]:
        def gt$(dd: Int) = greaterThan(getName(c), from(dd))
        def gte$(dd: Int) = greaterOrEqThan(getName(c), from(dd))
        def lt$(dd: Int) = lesserThan(getName(c), from(dd))
        def lte$(dd: Int) = lesserOrEqThan(getName(c), from(dd))
        def eq$(dd: Int) = equality(getName(c), from(dd))
        def in$(days: Set[Int]) = multiEquality(getName(c), days.map(from(_)).toSeq*)
        val descOrd: Order = desc(getName(c))
        val ascOrd: Order = asc(getName(c))
    case _: Pcolumn.Empty.type => EmptyTermOps
    // throw new NoSuchElementException

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

  object GT extends SortableNum[Long]:
    val columnName = getName(gameTime)
    val descOrd = desc(columnName)
    val ascOrd = asc(columnName)

  object HomeTeam extends FilterableChars[String]:
    val columnName = getName(hTeam)

    def eq$(stageName: String) = equality(columnName, from(stageName))

    def in$(stages: Set[String]) = multiEquality(columnName, stages.map(from(_)).toSeq*)

  final case class GenRecPb[T <: Tuple](private val content: Map[Pcolumn, Any]):
    def add[A](c: Pcolumn, value: A)(using tag: ClassTag[A]): GenRecPb[Column[T, A]] =
      GenRecPb(content.updated(c, value))

  object GenRecPb:
    val zero = GenRecPb[EmptyTuple](Map.empty)

  final case class GenRec[T <: Tuple](private val content: Map[Class[?], Any]):

    def add[A](value: A)(using tag: ClassTag[A]): GenRec[Column[T, A]] =
      GenRec(content.updated(tag.runtimeClass, value))

    inline def get[A](using tag: ClassTag[A]): A =
      inline if constValue[HasKey[T, A]] then content(tag.runtimeClass).asInstanceOf[A]
      else error("This content does not contain the requested type")

    inline def remove[A](using tag: ClassTag[A]): GenRec[Rem[T, A]] =
      inline if constValue[HasKey[T, A]] then GenRec[Rem[T, A]](content.removed(tag.runtimeClass))
      else error("This content does not contain the requested type")

  object GenRec:
    val zero = GenRec[EmptyTuple](Map.empty)

  final class A
  final class B
  final class C

  // runMain yoctodb.samples.GenericRecords2
  def main(args: Array[String]) =
    // GenRec.zero.add(GT).add(HomeTeam).get[HomeTeam.type].eq$("")
    // GenRec.zero.add(CEntry.GameDay()).add(CEntry.GameTime()).get[CEntry.GameDay].term.eq$(7)

    val st = column(stage)
    val b = column(aTeam)

    st.eq$("56")

    val dd = column(day)
    dd.lte$(78)

    // column(12)

    println("1 " + st.in$(Set("s", "b")))
    // println("2 " + a.lt$(4))
    println("3 " + b.eq$("lal"))

  end main

  /*
  GenRec.zero.add(A()).add(B()).term[B] // compiles

  GenRec.zero.add(A()).add(B()).term[A]

  GenRec.zero.add(A()).add(B()).remove[A]

  GenRec.zero.add(A()).add(B()).remove[A].remove[B]
   */

  // Record2.zero.put(A()).put(B()).remove[C].get[C] //fails to compile
