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

import GamesIndex._

//runMain yoctodb.Example
object Example extends App with StrictLogging {
  val indexPath = "indexes/games"

  val tzFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss Z")
  val EasternTime = java.time.ZoneId.of("America/New_York") //UTC-4
  //val MoscowTime = java.time.ZoneId.of("Europe/Moscow") //UTC+3.

  private def loadIndex(): Either[String, Database] = {
    val indexFile = Paths.get(indexPath).toFile
    if (indexFile.exists && indexFile.isFile) {
      val reader = DatabaseFormat.getCurrent.getDatabaseReader
      val db     = reader.from(Buffer.mmap(indexFile, false))
      logger.warn("* * * Index size: {} MB  * * *", indexFile.length() / (1024 * 1024))
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

        val payload: Buffer = yoctoDb.getFieldValue(docId, PayloadColumnName)
        val result          = NbaResultPB.parseFrom(new com.yandex.yoctodb.util.buf.BufferInputStream(payload))
        val when            = tzFormatter.format(ZonedDateTime.ofInstant(Instant.ofEpochMilli(result.when), EasternTime))
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
        val filterableSchema: Column[ColumnOps[_]] = rawFilterableSchema(filterableProtoc)
        val sortableSchema: Column[ColumnOps[_]]   = rawSortableSchema(sortableProtoc)

        logger.info("Filterable: {}", filterableSchema)
        logger.info("Sortable: {}", sortableSchema)

        if (filterableSchema.equals(Filterable) && sortableSchema.equals(Sortable)) {

          val yoctoQuery = sortableSchema.orderBy { rawSchema ⇒
            val gameTime = rawSchema.rawColumn[GameTime].term
            filterableSchema
              .where { schema ⇒
                val stage    = schema.rawColumn[FullStage].term
                val homeTeam = schema.rawColumn[HomeTeam].term
                val awayTeam = schema.rawColumn[AwayTeam].term

                yocto.select.where(
                  yocto.and(
                    stage.in$(Set("season-20-21")),
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
        } else
          throw new Exception(
            s"Schema mismatch: [Filterable: $filterableSchema : $Filterable] - [Sortable: $sortableSchema : $Sortable ]"
          )
    }

  def runValidated(): Unit = {

    val errors = List(
      validate(filterableProtoc, Filterable.columns, "Filterable"),
      validate(sortableProtoc, Sortable.columns, "Sortable")
    ).collect { case Some(err) ⇒ err }

    if (errors.isEmpty)
      loadIndex() match {
        case Left(err) ⇒ throw new Exception(err)
        case Right(yoctoDb) ⇒
          logger.info("Filterable: {}", Filterable)
          logger.info("Sortable: {}", Sortable)

          val yoctoQuery = Sortable.orderBy { s ⇒
            //val gameTime = s.column[GameTime].term
            val yyyy  = s.column[Year].term
            val month = s.column[Month].term
            val day   = s.column[Day].term

            Filterable
              .where { s ⇒
                val stage    = s.column[FullStage].term
                val homeTeam = s.column[HomeTeam].term
                val awayTeam = s.column[AwayTeam].term
                val winner   = s.column[GameWinner].term
                val mm       = s.column[Month].term

                yocto.select.where(
                  yocto.and(
                    stage.in$(Set("season-18-19", "season-19-20", "season-20-21")),
                    yocto.and(mm.gte$(1), mm.lte$(4)), //between 1 ... 4
                    yocto.or(
                      yocto.and(awayTeam.eq$("lal"), homeTeam.eq$("gsw")),
                      yocto.and(awayTeam.eq$("gsw"), homeTeam.eq$("lal"))
                    ),
                    winner.eq$("lal")
                  )
                )
              }
              .orderBy(yyyy.descOrd)
              .and(month.descOrd)
              .and(day.descOrd) //.limit(10) gameTime
          }
          exec(yoctoDb, yoctoQuery)
      }
    else throw new Exception(s"Schema mismatch: ${errors.mkString(",")}")
  }

  runValidated()

  runRaw()
}
