package yoctodb.samples

import java.net.URI
import java.nio.file.Path
import scala.annotation.targetName
import scala.compiletime.constValue
import scala.compiletime.error
import scala.compiletime.ops.boolean.&&
import scala.compiletime.ops.boolean.||

/*
An example use case: Enhanced builder pattern
https://mhammons.hashnode.dev/metadata-types-with-scala-3
 */

trait Data
trait StorageSystem:
  def load(key: String): Data
  def store(key: String, value: Data): Unit

class JDBCUri

class StorageSystemBuilder[M <: Tuple]:
  import StorageSystemBuilder.*

  def setStorage(p: Path): StorageSystemBuilder[
    FilesystemStorage *: RemoveAll[M, Tuple.Concat[SFTPSpecific, DBSpecific]]
  ] = StorageSystemBuilder()

  @targetName("dbStorage")
  def setStorage(p: JDBCUri): StorageSystemBuilder[
    DatabaseStorage *: RemoveAll[M, SFTPSpecific]
  ] = StorageSystemBuilder()

  @targetName("sftpStorage")
  def setStorage(p: URI): StorageSystemBuilder[
    SFTPStorage *: RemoveAll[M, DBSpecific]
  ] = StorageSystemBuilder()

  @targetName("sftpStorageComplete")
  def setStorage(
      p: URI,
      username: String,
      password: String,
    ): StorageSystemBuilder[SFTPStorage *: Credentials *: RemoveAll[M, DBSpecific]] = StorageSystemBuilder()

  inline def setCredentials(username: String, password: String): StorageSystemBuilder[Credentials *: M] =
    inline if constValue[Contains[M, SFTPStorage]]
    then StorageSystemBuilder()
    else error("Builder must be in sftp storage mode to use `setCredentials`")

  inline def noConnectionPool: StorageSystemBuilder[ConnectionPoolInfo *: M] =
    inline if constValue[Contains[M, DatabaseStorage]]
    then StorageSystemBuilder()
    else error("This setting can only be used with DatabaseStorage")

  @targetName("usingConnectionPool")
  inline def setConnectionPool(
      connectionPoolClass: Class[?]
    ): StorageSystemBuilder[ConnectionPoolInfo *: M] =
    inline if constValue[Contains[M, DatabaseStorage]]
    then StorageSystemBuilder()
    else error("This setting can only be used with Database storage")

  def setCacheInfo(cached: false): StorageSystemBuilder[CacheInfo *: M] =
    StorageSystemBuilder()

  @targetName("needsCache")
  def setCacheInfo(
      cached: true,
      syncRateInMs: Long,
    ): StorageSystemBuilder[CacheInfo *: M] = StorageSystemBuilder()

  inline def build(): StorageSystem =
    inline if constValue[
        SetEquals[M, CompleteFS] || SetEquals[M, CompleteDB] || SetEquals[M, CompleteSFTP]
      ]
    then
      new StorageSystem:
        def load(key: String): Data = ???
        def store(key: String, data: Data) = ()
    else error("Cannot build. The builder is currently in an incomplete state")

object StorageSystemBuilder:

  val zero: StorageSystemBuilder[EmptyTuple] = StorageSystemBuilder()

  type SFTPSpecific = (SFTPStorage, Credentials)
  type DBSpecific = (DatabaseStorage, ConnectionPoolInfo)

  type CompleteFS = (FilesystemStorage, CacheInfo)
  type CompleteDB = (DatabaseStorage, ConnectionPoolInfo, CacheInfo)
  type CompleteSFTP = (SFTPStorage, Credentials, CacheInfo)

  type Remove[T <: Tuple, U] <: Tuple = T match
    case U *: t     => Remove[t, U]
    case h *: t     => h *: Remove[t, U]
    case EmptyTuple => EmptyTuple

  type RemoveAll[T <: Tuple, U <: Tuple] <: Tuple = U match
    case h *: t     => RemoveAll[Remove[T, h], t]
    case EmptyTuple => T

  type Contains[T <: Tuple, U] <: Boolean = T match
    case U *: ?     => true
    case ? *: t     => Contains[t, U]
    case EmptyTuple => false

  type IsSubsetOrEqualTo[T <: Tuple, U <: Tuple] <: Boolean = T match
    case h *: t     => Contains[U, h] && IsSubsetOrEqualTo[t, U]
    case EmptyTuple => true

  type SetEquals[T <: Tuple, U <: Tuple] = IsSubsetOrEqualTo[T, U] && IsSubsetOrEqualTo[U, T]

  sealed trait DatabaseStorage
  sealed trait FilesystemStorage
  sealed trait SFTPStorage
  sealed trait Credentials
  sealed trait ConnectionPoolInfo
  sealed trait CacheInfo

object Runner:

  def main(args: Array[String]): Unit =
    StorageSystemBuilder.zero.setStorage(JDBCUri()).noConnectionPool.setCacheInfo(false).build()

    // StorageSystemBuilder.init.noConnectionPool //compile error: "This setting can only be used with DatabaseStorage"

    // StorageSystemBuilder.zero.setStorage(java.nio.file.Path.of("bar")).setCredentials("foo", "baz") //compile error: Builder must be in sftp storage mode to use `setCredentials`

    // StorageSystemBuilder.init.setStorage(JDBCUri()).noConnectionPool.build() //compile error: Cannot build. The builder is currently in an incomplete state

  end main
