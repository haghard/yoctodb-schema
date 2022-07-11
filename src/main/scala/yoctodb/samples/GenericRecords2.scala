package yoctodb.samples

import scala.compiletime.constValue
import scala.compiletime.error
import scala.reflect.ClassTag
import scala.compiletime.ops.boolean.{ &&, || }

//https://mhammons.hashnode.dev/metadata-types-with-scala-3
object GenericRecords2:

  type Contains[T <: Tuple, U] <: Boolean = T match
    case U *: r     => true
    case ? *: r     => Contains[r, U]
    case EmptyTuple => false

  type Rem[T <: Tuple, U] <: Tuple = T match
    case U *: r     => Rem[r, U]
    case t *: r     => t *: Rem[r, U]
    case EmptyTuple => EmptyTuple

  type Add[T <: Tuple, U] = U *: Rem[T, U]

  final case class GenRec[T <: Tuple](private val content: Map[Class[?], Any]):

    def +[A](value: A)(using tag: ClassTag[A]): GenRec[Add[T, A]] =
      GenRec(content.updated(tag.runtimeClass, value))

    inline def get[A](using tag: ClassTag[A]): A =
      inline if constValue[Contains[T, A]] then content(tag.runtimeClass).asInstanceOf[A]
      else error("This content does not contain the requested type")

    inline def remove[A](using tag: ClassTag[A]): GenRec[Rem[T, A]] =
      inline if constValue[Contains[T, A]] then GenRec[Rem[T, A]](content.removed(tag.runtimeClass))
      else error("This content does not contain the requested type")

  object GenRec:
    val zero = GenRec[EmptyTuple](Map.empty)

  final class A
  final class B
  final class C

  GenRec.zero.+(A()).+(B()).get[B] // compiles

  GenRec.zero.+(A()).+(B()).get[A]

  GenRec.zero.+(A()).+(B()).remove[A]
  GenRec.zero.+(A()).+(B()).remove[A].remove[B]

  // Record2.zero.put(A()).put(B()).remove[C].get[C] //fails to compile
