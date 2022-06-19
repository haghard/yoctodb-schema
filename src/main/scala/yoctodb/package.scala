// Copyright (c) 2021-22 by Vadim Bondarev
// This software is licensed under the Apache License, Version 2.0.
// You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0.

import com.yandex.yoctodb.v1.immutable.V1Database
import mazboot.validations.FromPredicate
import mazboot.validations.strings.MatchesRegex
import yoctodb.GamesIndex.FilterableSegment
import yoctodb.GamesIndex.SortableSegment
import yoctodb.GamesIndex.checkFilteredSegment
import yoctodb.GamesIndex.checkSortedSegment

package object yoctodb:

  val EmptyColumn = "empty"

  private class TeamRegexInvariant()
      extends FromPredicate[String](
        _.matches("lal|lac|por|chi|sea|hou|mia|okc|den|mil|ind|atl|min|tor|gsw"),
        "Team match pattern error !",
      )

  private class StageRegexInvariant()
      extends FromPredicate[String](_.matches("(season|playoff)-[0-9]{2}-[0-9]{2}"), "Stage match pattern error !")

  private class FilterableSchemaInvariant()
      extends FromPredicate[V1Database](
        { yoctoDb => checkFilteredSegment(yoctoDb, FilterableSegment.columns) },
        "Filterable schema region mismatch !",
      )

  private class SortableSchemaInvariant()
      extends FromPredicate[V1Database](
        { yoctoDb => checkSortedSegment(yoctoDb, SortableSegment.columns) },
        "Sortable schema region mismatch !",
      )

  val Team = TeamRegexInvariant()
  type Team = Team.Valid
  type TeamErr = Team.Error

  val Stage = StageRegexInvariant()
  type Stage = Stage.Valid
  type StageErr = Stage.Error

  val FSchema = FilterableSchemaInvariant()
  type FSchema = FSchema.Valid
  type FSchemaErr = FSchema.Error

  val SSchema = SortableSchemaInvariant()
  type SSchema = SSchema.Valid
  type SSchemaErr = SSchema.Error
