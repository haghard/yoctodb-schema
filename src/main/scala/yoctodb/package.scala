// Copyright (c) 2021 by Vadim Bondarev
// This software is licensed under the Apache License, Version 2.0.
// You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0.

import zio.prelude.Assertion.*
import zio.prelude.Subtype
import zio.prelude.Validation
import zio.prelude.*

package object yoctodb:

  val EmptyColumn = "empty"

  object Team extends Subtype[String]:
    inline override def assertion =
      matches("lal|lac|por|chi|sea|hou|mia|okc|den|mil|ind|atl|min|tor|gsw")

  type Team = Team.Type

  object Stage extends Subtype[String]:
    inline override def assertion =
      matches("(season|playoff)-[0-9]{2}-[0-9]{2}")

  type Stage = Stage.Type
