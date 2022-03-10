// Copyright (c) 2021-22 by Vadim Bondarev
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

import java.nio.file.Paths
import java.time.Instant
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import scala.util.Try

import GamesIndex.*
import CEntry.*

import mazboot.net.*
import mazboot.validations.*
import mazboot.validations.strings.StartsWith
import mazboot.validations.strings.MatchesRegex
import mazboot.ints.{ GreaterThanOrEqualsOne, Positive }

//comment it before test:compile
@main def app(): Unit = Example()

object Example:

  val logger = LoggerFactory.getLogger("app")

  val tzFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss Z")
  val EasternTime = java.time.ZoneId.of("America/New_York") // UTC-4
  // val MoscowTime = java.time.ZoneId.of("Europe/Moscow") //UTC+3.

  def loadIndex(): Either[Throwable, V1Database] =
    Try {
      val indexPath = "indexes/games"
      val indexFile = Paths.get(indexPath).toFile
      if (indexFile.exists && indexFile.isFile)
        val reader = DatabaseFormat.getCurrent.getDatabaseReader
        val db = reader.from(Buffer.mmap(indexFile, false))
        logger.warn("★ ★ ★ Index size: {} MB  ★ ★ ★", indexFile.length() / (1024 * 1024))
        logger.warn("★ ★ ★ Docs number: {} ★ ★ ★", db.getDocumentCount)
        db.asInstanceOf[V1Database]
      else throw new Exception(s"Couldn't find or open file $indexPath")
    }.toEither

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

  final case class Params(
      stageA: Stage,
      stageB: Stage,
      teamA: Team,
      teamB: Team)

  def validateInputs() = for {
    (ses18_19, ses19_20) <- bothStages("season-18-19", "season-19-20")
    (home, away) <- bothTeams("lal", "gsw")
  } yield Params(ses18_19, ses19_20, home, away)

  def apply(): Unit =
    (for {
      index <- loadIndex()
      _ <- validateSchema(index)
      _ <- validateInputs().map(p => runQuery(index, p.stageA, p.stageB, p.teamA, p.teamB))
    } yield ()) match
      case Right(_) =>
        logger.warn("★ ★ ★ Success ★ ★ ★")
      case Left(err) =>
        err match
          case th: Throwable =>
            logger.error("★ ★ Exception: ", th)
          case list: List[?] =>
            logger.error(s"★ ★ ★ Error: [${list.mkString(",")}]")

  def runQuery(
      index: V1Database,
      ses18_19: Stage,
      ses19_20: Stage,
      teamA: Team,
      teamB: Team,
    ) =
    logger.warn("★ ★ ★ Index schema layout ★ ★ ★")
    logger.info(showSchema(FilterableSegment.columns ++ SortableSegment.columns))
    logger.warn("★ ★ ★ Index schema validated successfully ★ ★ ★")

    val yoctoQuery = GamesIndex.SortableSegment.orderBy { s =>
      val gameTime = s.column[GameTime].term
      // val yyyy = s.column[GameYear].term
      // val month = s.column[GameMonth].term
      // val day = s.column[GameDay].term

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
                yocto.or(homeTeam.eq$(teamA), awayTeam.eq$(teamA)),
              )
            )
        }
        .orderBy(gameTime.descOrd)
        .limit(10)
      // .orderBy(yyyy.descOrd)
      // .and(month.descOrd)
      // .and(day.descOrd)
    }

    exec(index, yoctoQuery)
