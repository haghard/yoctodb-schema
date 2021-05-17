// Copyright (c) 2021 by Vadim Bondarev
// This software is licensed under the Apache License, Version 2.0.
// You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0.

package yoctodb

import com.typesafe.scalalogging.StrictLogging
import com.yandex.yoctodb.DatabaseFormat
import com.yandex.yoctodb.immutable.Database
import com.yandex.yoctodb.query.{QueryBuilder ⇒ yocto}
import com.yandex.yoctodb.util.buf.Buffer
import yoctodb.schema.games.v1.NbaResultPB

import java.nio.file.Paths
import java.time.Instant
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import scala.collection.immutable.Vector

import GamesIndex._

//runMain yoctodb.Example
object Example extends App with StrictLogging {
  val indexPath = "indices/games"

  val tzFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss Z")
  val EasternTime = java.time.ZoneId.of("America/New_York") //UTC-4
  //val MoscowTime = java.time.ZoneId.of("Europe/Moscow") //UTC+3.

  private def loadIndex(): Either[String, Database] = {
    val indexFile = Paths.get(indexPath).toFile
    if (indexFile.exists && indexFile.isFile) {
      val reader = DatabaseFormat.getCurrent.getDatabaseReader
      val db     = reader.from(Buffer.mmap(indexFile, false))
      logger.warn(s"* * * Index size: ${indexFile.length() / (1024 * 1024)} MB  * * *")
      logger.warn("* * * Docs number: {} * * *", db.getDocumentCount)
      Right(db)
    } else Left(s"Couldn't find or open file $indexPath")
  }

  private def exec(yoctoDb: Database, yoctoQuery: com.yandex.yoctodb.query.Query) =
    yoctoDb.execute(
      yoctoQuery,
      (docId: Int, _: Database) ⇒ {

        //val payload: com.yandex.yoctodb.util.buf.Buffer = yoctoDb.getFieldValue(docId, InfoColumnName)
        //val result                                      = new String(payload.toByteArray)

        val payload: com.yandex.yoctodb.util.buf.Buffer = yoctoDb.getFieldValue(docId, PayloadColumnName)
        val result                                      = NbaResultPB.parseFrom(new com.yandex.yoctodb.util.buf.BufferInputStream(payload))
        val when                                        = tzFormatter.format(ZonedDateTime.ofInstant(Instant.ofEpochMilli(result.when), EasternTime))
        logger.debug(s"DocId: $docId")
        logger.debug(s"When: $when")
        logger.debug(result.toProtoString)
        //logger.debug(result)
        true
      }
    )

  def runRaw() =
    loadIndex() match {
      case Left(err) ⇒ throw new Exception(err)
      case Right(yoctoDb) ⇒
        val filteredSchema: SchemaColumn[ColumnOps[_]] = rawFilterableSchema(filterableProtoc)
        val sortedSchema: SchemaColumn[ColumnOps[_]]   = rawSortableSchema(sortableProtoc)

        logger.info("Filtered: {}", filteredSchema.columnNames.mkString(","))
        logger.info("Sorted: {}", sortedSchema.columnNames.mkString(","))

        val yoctoQuery = sortedSchema.orderBy { phantomSchema ⇒
          val gameTime = phantomSchema.rawColumn[GameTime].term
          filteredSchema
            .where { schema ⇒
              val stage    = schema.rawColumn[FullStage].term
              val homeTeam = schema.rawColumn[HomeTeam].term
              val awayTeam = schema.rawColumn[AwayTeam].term

              yocto.select.where(
                yocto.and(
                  stage.in$(Vector("season-20-21")),
                  yocto.or(
                    yocto.and(awayTeam.eq$("lal"), homeTeam.eq$("gsw")),
                    yocto.and(awayTeam.eq$("gsw"), homeTeam.eq$("lal"))
                  )
                )
              )
            /*yocto.select.where(
                yocto.and(
                  stage.eq$("season-20-21"),
                  yocto.or(homeTeam.eq$("lal"), awayTeam.eq$("lal"))
                )
              )*/
            }
            .orderBy(gameTime.descOrd)
            .limit(10)
        }

        exec(yoctoDb, yoctoQuery)
    }

  def runValidated(): Unit =
    if (
      validate(filterableProtoc, Filterable.columnNames) &&
      validate(sortableProtoc, Sortable.columnNames)
    ) {
      loadIndex() match {
        case Left(err) ⇒ throw new Exception(err)
        case Right(yoctoDb) ⇒
          val yoctoQuery = Sortable.orderBy { s ⇒
            val gameTime = s.column[GameTime].term

            Filterable
              .where { s ⇒
                val stage    = s.column[FullStage].term
                val homeTeam = s.column[HomeTeam].term
                val awayTeam = s.column[AwayTeam].term
                val winner   = s.column[GameWinner].term

                yocto.select.where(
                  yocto.and(
                    stage.in$(Vector("season-20-21")),
                    yocto.or(
                      yocto.and(awayTeam.eq$("lal"), homeTeam.eq$("gsw")),
                      yocto.and(awayTeam.eq$("gsw"), homeTeam.eq$("lal"))
                    ),
                    winner.eq$("lal")
                  )
                )
              }
              .orderBy(gameTime.descOrd) //.limit(10)
          }
          exec(yoctoDb, yoctoQuery)
      }
    } else throw new Exception("Schema mismatch !")

  runValidated()
  //runRaw()
}
