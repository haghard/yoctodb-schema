// Copyright (c) 2021 by Vadim Bondarev
// This software is licensed under the Apache License, Version 2.0.
// You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0.

package yoctodb

import yoctodb.schema.games.v1.GamesSchema
import yoctodb.schema.games.v1.GamesSchema.Index

trait ColumnOps[A]:

  def term: TermOps[A]

  def index: GamesSchema.Index

  def fieldName = parse(index)

  private def parse(ind: GamesSchema.Index): String =
    ind match
      case Index.Stage(v)    => v.companion.scalaDescriptor.name
      case Index.AwayTeam(v) => v.companion.scalaDescriptor.name
      case Index.HomeTeam(v) => v.companion.scalaDescriptor.name
      case Index.Time(v)     => v.companion.scalaDescriptor.name
      case Index.Winner(v)   => v.companion.scalaDescriptor.name
      case Index.Year(v)     => v.companion.scalaDescriptor.name
      case Index.Month(v)    => v.companion.scalaDescriptor.name
      case Index.Day(v)      => v.companion.scalaDescriptor.name
      //case Index.Fake(v)     ⇒ v.companion.scalaDescriptor.name
      case Index.Empty => EmptyColumn
