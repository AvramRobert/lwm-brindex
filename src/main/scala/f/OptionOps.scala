package f

import scala.annotation.tailrec
import scala.collection.GenTraversable

object OptionOps {

  def sequence[A](v: GenTraversable[Option[A]]): Option[GenTraversable[A]] = {
    @tailrec
    def go(toGo: GenTraversable[Option[A]], soFar: GenTraversable[A]): Option[GenTraversable[A]] = {
      if(toGo.isEmpty) Some(soFar)
      else {
        toGo.head match {
          case Some(h) => go(toGo.tail, soFar ++ GenTraversable(h))
          case _ => None
        }
      }
    }
    go(v, GenTraversable())
  }
}
