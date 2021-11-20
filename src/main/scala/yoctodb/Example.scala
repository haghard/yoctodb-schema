// Copyright (c) 2021 by Vadim Bondarev
// This software is licensed under the Apache License, Version 2.0.
// You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0.

package yoctodb

import com.yandex.yoctodb.DatabaseFormat
import com.yandex.yoctodb.immutable.Database
import com.yandex.yoctodb.query.QueryBuilder as yocto
import com.yandex.yoctodb.util.buf.Buffer
import com.yandex.yoctodb.v1.immutable.V1Database
import org.slf4j.LoggerFactory
import yoctodb.schema.games.v1.NbaResultPB
import zio.prelude.*

import java.nio.file.Paths
import java.time.Instant
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import scala.util.Try

import GamesIndex.*
import CEntry.*

//comment it before test:compile
@main def app(): Unit = Example()

object Example:

  val logger = LoggerFactory.getLogger("app")

  val tzFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss Z")
  val EasternTime = java.time.ZoneId.of("America/New_York") // UTC-4
  // val MoscowTime = java.time.ZoneId.of("Europe/Moscow") //UTC+3.

  def loadIndex(): Validation[String, V1Database] =
    Validation
      .fromTry(
        Try {
          val indexPath = "indexes/games"
          val indexFile = Paths.get(indexPath).toFile
          if (indexFile.exists && indexFile.isFile) {
            val reader = DatabaseFormat.getCurrent.getDatabaseReader
            val db = reader.from(Buffer.mmap(indexFile, false))
            logger.warn("★ ★ ★ Index size: {} MB  ★ ★ ★", indexFile.length() / (1024 * 1024))
            logger.warn("★ ★ ★ Docs number: {} ★ ★ ★", db.getDocumentCount)
            db.asInstanceOf[V1Database]
          }
          else throw new Exception(s"Couldn't find or open file $indexPath")
        }
      )
      .mapError(_.getMessage)

  def exec(yoctoDb: V1Database, yoctoQuery: com.yandex.yoctodb.query.Query) =
    yoctoDb.execute(
      yoctoQuery,
      (docId: Int, _: Database) => {
        // val payload: com.yandex.yoctodb.util.buf.Buffer = yoctoDb.getFieldValue(docId, InfoColumnName)
        // val result                                      = new String(payload.toByteArray)
        // logger.debug(result)

        val payload: Buffer = yoctoDb.getFieldValue(docId, PayloadColumnName)
        val result =
          NbaResultPB.parseFrom(new com.yandex.yoctodb.util.buf.BufferInputStream(payload))
        val when = tzFormatter.format(
          ZonedDateTime.ofInstant(Instant.ofEpochMilli(result.when), EasternTime)
        )

        logger.debug(s"DocId: $docId")
        logger.debug(s"Ts: $when")
        logger.debug(result.toProtoString)
        true
      },
    )

  /** Why prefer zio.Validation over Either? Either short-circuits on failure. If validation errors occur, we just get
    * the first one. For more information see: https://youtu.be/Hj4fRrlKHeY. You can go for Either[NonEmptyList[String],
    * Unit] but it requires boilerplate.
    */
  def isValidSchema(yoctoDb: V1Database): Validation[String, Boolean] =
    Validation.validateWith(
      Validation.fromPredicateWith("Filterable schema region mismatch !")(
        checkFilteredSegment(yoctoDb, FilterableSegment.columns)
      )(
        identity
      ),
      Validation.fromPredicateWith("Sortable schema region mismatch !")(
        checkSortedSegment(yoctoDb, SortableSegment.columns)
      )(
        identity
      ),
    )(_ && _)

  def apply(): Unit =
    Validation
      .validateWith(
        loadIndex(),
        stage("season-18-19"),
        stage("season-19-20"),
        team("lal"),
        team("gsw"),
      ) { (yoctoDb, ses18_19, ses19_20, lal, gsw) =>
        (yoctoDb, ses18_19, ses19_20, lal, gsw)
      }
      .flatMap {
        case (yoctoDb, ses18_19, ses19_20, lal, gsw) =>
          logger.warn("★ ★ ★ Index schema layout ★ ★ ★")
          logger.info(showSchema(FilterableSegment.columns ++ SortableSegment.columns))
          isValidSchema(yoctoDb).map(_ => (yoctoDb, ses18_19, ses19_20, lal, gsw))
      } match
      case ZValidation.Failure(_, errors) =>
        logger.error(errors.toChunk.mkString(","))
      case ZValidation.Success(_, params) =>
        logger.warn("★ ★ ★ Index schema validated successfully ★ ★ ★")

        val (yoctoDb, ses18_19, ses19_20, lal, gsw) = params

        val yoctoQuery = GamesIndex.SortableSegment.orderBy { s =>
          val gameTime = s.column[GameTime].term
          // val yyyy = s.column[Year].term
          // val month = s.column[Month].term
          // val day = s.column[Day].term

          GamesIndex
            .FilterableSegment
            .where { s =>
              val stage = s.column[GameFullStage].term
              val homeTeam = s.column[GameHomeTeam].term
              val awayTeam = s.column[GameAwayTeam].term
              val winner = s.column[GameWinner].term

              yocto
                .select
                .where(
                  yocto.and(
                    stage.in$(Set(ses18_19, ses19_20)),
                    yocto.or(homeTeam.eq$(lal), awayTeam.eq$(lal)),
                  )
                )
            }
            .orderBy(gameTime.descOrd)
            .limit(10)
        // .orderBy(yyyy.descOrd)
        // .and(month.descOrd)
        // .and(day.descOrd)
        }

        exec(yoctoDb, yoctoQuery)
