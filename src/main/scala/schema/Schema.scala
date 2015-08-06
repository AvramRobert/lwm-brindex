package schema

object Schema {

  def empty[A, B]: AbstractSchema[A, B] = new AbstractSchema[A, B] {
    override def rules: Map[A, B] = Map()

    override def order: IndexedSeq[A] = IndexedSeq()
  }
}

trait AbstractSchema[A, +B] {
  def keywords: Vector[A] = rules.keys.toVector

  def rules: Map[A, B]

  def order: IndexedSeq[A]

  implicit val ordering: Ordering[A] = Ordering.by(order.indexOf)
}