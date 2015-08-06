package f

trait Monoid[A] {
  def append(v1: A, v2: => A): A
  def zero: A
}
