package indexer

import schema.AbstractSchema


trait Transient[A, +B] { self: Schematic[_, _] =>

  def apply(): trans.Transition.Transition[A, B] = transition

  def transition: trans.Transition.Transition[A, B]
}


trait Schematic[A, B] {
  implicit val schema: AbstractSchema[A, B]
}

