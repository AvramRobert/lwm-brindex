package indexer

import schema.AbstractSchema
import trans.Transition.->


trait Transient[A, +B] { self: Schematic[_, _] =>

  def apply(): A -> B = transition

  def transition: A -> B
}


trait Schematic[A, B] {
  implicit val schema: AbstractSchema[A, B]
}

