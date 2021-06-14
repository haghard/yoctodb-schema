// Copyright (c) 2021 by Vadim Bondarev
// This software is licensed under the Apache License, Version 2.0.
// You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0.

import zio.prelude.*
import zio.test.Assertion.*

package object yoctodb:

  val EmptyColumn = "empty"

  object Team
      extends SubtypeSmart[String](
        matchesRegex("lal|lac|por|chi|sea|hou|mia|okc|den|mil|ind|atl|min|tor|gsw")
      )
  type Team = Team.Type

  object Stage extends SubtypeSmart[String](matchesRegex("(season|playoff)-[0-9]{2}-[0-9]{2}"))
  type Stage = Stage.Type

//implicitly[String <:< CharSequence]
