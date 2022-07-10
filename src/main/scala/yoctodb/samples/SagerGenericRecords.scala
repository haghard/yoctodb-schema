package yoctodb.samples

import com.github.mvv.sager.*
import yoctodb.schema.games.v1.GamesSchema
import yoctodb.schema.games.v1.GamesSchema.*

/** https://github.com/mvv/sager
  */
object SagerGenericRecords:

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

  def main(args: Array[String]): Unit =

    val row =
      Field[Pcolumn.Stage](stage)
        .add[Pcolumn.AwayTeam](aTeam)
        .add[Pcolumn.HomeTeam](hTeam)
        .add[Pcolumn.Time](gameTime)
        .add[Pcolumn.Winner](winner)
        .add[Pcolumn.Year](year)
        .add[Pcolumn.Month](month)
        .add[Pcolumn.Day](day)

    println(row)
    println("***********************")
    println(row.remove[Pcolumn.Stage])
    println("***********************")

    println(row.get[Pcolumn.Day])
    println(row.get[Pcolumn.Stage])

  end main
