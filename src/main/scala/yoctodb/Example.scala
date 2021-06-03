// Copyright (c) 2021 by Vadim Bondarev
// This software is licensed under the Apache License, Version 2.0.
// You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0.

package yoctodb

import com.typesafe.scalalogging.StrictLogging
import com.yandex.yoctodb.DatabaseFormat
import com.yandex.yoctodb.immutable.Database
import com.yandex.yoctodb.query.{QueryBuilder ⇒ yocto}
import com.yandex.yoctodb.util.buf.Buffer
import com.yandex.yoctodb.v1.immutable.V1Database
import yoctodb.schema.games.v1.NbaResultPB
import zio.prelude._

import java.nio.file.Paths
import java.time.Instant
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter

import GamesIndex._

//runMain yoctodb.Example
object Example extends App with StrictLogging {
  val indexPath = "indexes/games"

  val tzFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss Z")
  val EasternTime = java.time.ZoneId.of("America/New_York") //UTC-4
  //val MoscowTime = java.time.ZoneId.of("Europe/Moscow") //UTC+3.

  private def loadIndex(): Validation[String, V1Database] = {
    val indexFile = Paths.get(indexPath).toFile
    if (indexFile.exists && indexFile.isFile) {
      val reader = DatabaseFormat.getCurrent.getDatabaseReader
      val db     = reader.from(Buffer.mmap(indexFile, false))
      logger.warn("* * * Index size: {} MB  * * *", indexFile.length() / (1024 * 1024))
      logger.warn("* * * Docs number: {} * * *", db.getDocumentCount)
      Validation.succeed(db.asInstanceOf[V1Database])
    } else Validation.fail(s"Couldn't find or open file $indexPath")
  }

  private def exec(yoctoDb: V1Database, yoctoQuery: com.yandex.yoctodb.query.Query) =
    yoctoDb.execute(
      yoctoQuery,
      (docId: Int, _: Database) ⇒ {
        //val payload: com.yandex.yoctodb.util.buf.Buffer = yoctoDb.getFieldValue(docId, InfoColumnName)
        //val result                                      = new String(payload.toByteArray)
        //logger.debug(result)

        val payload: Buffer = yoctoDb.getFieldValue(docId, PayloadColumnName)
        val result          = NbaResultPB.parseFrom(new com.yandex.yoctodb.util.buf.BufferInputStream(payload))
        val when            = tzFormatter.format(ZonedDateTime.ofInstant(Instant.ofEpochMilli(result.when), EasternTime))
        logger.debug(s"DocId: $docId")
        logger.debug(s"Ts: $when")
        logger.debug(result.toProtoString)
        true
      }
    )

  def run() =
    Validation
      .validateWith(loadIndex(), stage("season-18-19"), stage("season-19-20"), team("lal"), team("gsw")) {
        (yoctoDb, ses18_19, ses19_20, lal, gsw) ⇒
          logger.info("Schema {}", showSchema(Filterable.columns ++ Sortable.columns))

          if (checkFilteredSegment(yoctoDb, Filterable.columns) && checkSortedSegment(yoctoDb, Sortable.columns)) {
            val yoctoQuery = GamesIndex.Sortable.orderBy { s ⇒
              //val gameTime = s.column[GameTime].term
              val yyyy  = s.column[Year].term
              val month = s.column[Month].term
              val day   = s.column[Day].term

              GamesIndex.Filterable
                .where { s ⇒
                  val stage    = s.column[FullStage].term
                  val homeTeam = s.column[HomeTeam].term
                  val awayTeam = s.column[AwayTeam].term
                  val winner   = s.column[GameWinner].term
                  val mm       = s.column[Month].term

                  yocto.select.where(
                    yocto.and(
                      stage.in$(Set(ses18_19, ses19_20)),
                      yocto.and(mm.gte$(1), mm.lte$(4)), //between 1 ... 4
                      yocto.or(
                        yocto.and(awayTeam.eq$(lal), homeTeam.eq$(gsw)),
                        yocto.and(awayTeam.eq$(gsw), homeTeam.eq$(lal))
                      ),
                      winner.eq$(lal)
                    )
                  )
                }
                //.orderBy(gameTime.descOrd)
                .orderBy(yyyy.descOrd)
                .and(month.descOrd)
                .and(day.descOrd) //.limit(10) gameTime
            }
            exec(yoctoDb, yoctoQuery)
          }
      }

  run() match {
    case ZValidation.Failure(_, errors) ⇒
      logger.error(errors.toChunk.mkString(","))
    case ZValidation.Success(_, _) ⇒
      logger.warn("Success")
  }
}
