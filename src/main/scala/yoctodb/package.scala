// Copyright (c) 2021-22 by Vadim Bondarev
// This software is licensed under the Apache License, Version 2.0.
// You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0.

import com.yandex.yoctodb.v1.immutable.V1Database
import mazboot.validations.FromPredicate
import mazboot.validations.strings.MatchesRegex
import yoctodb.GamesIndex.{ FilterableSegment, SortableSegment, checkFilteredSegment, checkSortedSegment }

package object yoctodb:

  val EmptyColumn = "empty"

  open private class TeamRegex()
      extends FromPredicate[String](
        _.matches("lal|lac|por|chi|sea|hou|mia|okc|den|mil|ind|atl|min|tor|gsw"),
        "Team match pattern error !",
      )

  open private class StageRegex()
      extends FromPredicate[String](_.matches("(season|playoff)-[0-9]{2}-[0-9]{2}"), "Stage match pattern error !")

  open private class FilterableSchema()
      extends FromPredicate[V1Database](
        { yoctoDb => checkFilteredSegment(yoctoDb, FilterableSegment.columns) },
        "Filterable schema region mismatch !",
      )

  open private class SortableSchema()
      extends FromPredicate[V1Database](
        { yoctoDb => checkSortedSegment(yoctoDb, SortableSegment.columns) },
        "Sortable schema region mismatch !",
      )

  val Team = TeamRegex()
  type Team = Team.Valid
  type TeamErr = Team.Error

  val Stage = StageRegex()
  type Stage = Stage.Valid
  type StageErr = Stage.Error

  val FSchema = FilterableSchema()
  type FSchema = FSchema.Valid
  type FSchemaErr = FSchema.Error

  val SSchema = SortableSchema()
  type SSchema = SSchema.Valid
  type SSchemaErr = SSchema.Error
