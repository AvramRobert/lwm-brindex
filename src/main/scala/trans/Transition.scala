package trans

import f.Monoid

import scala.util.Try

object Transition { self =>

  type ->[A, +B] = A => Result[B]
  
  def success[A, B](b: => B): A -> B = a => Success(b)

  def failed[A, B](msg: String): A -> B = a => Failure(msg)

  def parse[A, B, C](i: A -> B)(parser: B -> C): A -> C = compose(i, parser)

  def run[A, B](i: A -> B, input: A) =  i(input)

  def map[A, B, C](i: A -> B)(f: B => C): A -> C = a => {
    flatMap(i)(b => success(f(b)))(a)
  }

  def flatMap[A, B, C](i: A -> B)(f: B => A -> C): A -> C = a => i(a) match {
    case Success(v) => f(v)(a)
    case Failure(e) => Failure(e)
  }

  def foldLeft[A, B, C](i: A -> B)(z: C)(f: (=> C, B) => C): A -> C = map(i)(b => f(z, b))

  def foldRight[A, B, C](i: A -> B)(z: C)(f: (B, => C) => C): A -> C = a => i(a).map(b => f(b, z))

  def map2[A, B, C, D](i1: A -> B, i2: B -> C)(f: (B, C) => D): A -> D = a => for {
    b <- i1(a)
    c <- i2(b)
  } yield f(b, c)

  def compose[A, B, C](i1: A -> B, i2: B -> C): A -> C = a => i1(a).flatMap(b => i2(b))

  def product[A, B, C](i1: A -> B, i2: A -> C): A -> (B, C) = a => for {
    b <- i1(a)
    c <- i2(a)
  } yield (b, c)


  def foldMap[A, B, C](i: A -> B)(mb: Monoid[C])(f: B => C): A -> C = foldLeft(i)(mb.zero)((c, b) => mb.append(c, f(b)))

  def fold[A, B](i: A -> B)(z: B)(f: (B, => B) => B): A -> B = map(i)(b => f(z, b))

  def and[A, B, C](i1: A -> B, i2: => B -> C): A -> C = a => i1(a) match {
    case Success(b) => i2(b)
    case Failure(e) => Failure(e)
  }

  def contained[A, B](i: A -> B)(a: A): Result[B] = Try(i(a)) match {
    case util.Success(rb) => rb
    case util.Failure(e) => Failure(e.getMessage)
  }

  implicit def conversion[A, B](i: A -> B): TransitionOps[A, B] = new TransitionOps[A, B](i)


  class TransitionOps[A, B](i: A -> B) {

    def map[C](f: B => C): A -> C = self.map(i)(f)

    def flatMap[C](f: B => A -> C): A -> C = self.flatMap(i)(f)

    def foldLeft[C](z: C)(f: (=> C, B) => C): A -> C = self.foldLeft(i)(z)(f)

    def foldRight[C](z: C)(f: (B, => C) => C): A -> C = self.foldRight(i)(z)(f)

    def compose[C](i2: B -> C): A -> C = self.compose(i, i2)

    def product[C](i2: A -> C): A -> (B, C) = self.product(i, i2)

    def ~>[C](i2: B -> C): A -> C = compose(i2)

    def **[C](i2: A -> C): A -> (B, C) = product(i2)

    def foldMap[C](f: B => C)(mb: Monoid[C]): A -> C = self.foldMap(i)(mb)(f)

    def fold(z: B)(f: (B, => B) => B): A -> B = self.fold(i)(z)(f)

    def fold(mb: Monoid[B]): A -> B = self.fold(i)(mb.zero)(mb.append)

    def parse[C](implicit parser: B -> C): A -> C = self.parse(i)(parser)

    def &[C] (i2: => B -> C): A -> C = and(i2)

    def and[C](i2: => B -> C): A -> C = self.and(i, i2)

    def contained(a: A): Result[B] = self.contained(i)(a)
  }
}

sealed trait Result[+A] {
  def map[B](f: A => B): Result[B] = this match {
    case Success(v) => Success(f(v))
    case Failure(e) => Failure(e)
  }

  def flatMap[B](f: A => Result[B]): Result[B] = this match {
    case Success(v) => f(v)
    case Failure(e) => Failure(e)
  }

  def isSuccess: Boolean = this match {
    case Success(v) => true
    case _ => false
  }

  def isFailure: Boolean = this match {
    case Success(v) => true
    case _ => false
  }

  override def toString: String = this match {
    case Success(v) => s"Success($v)"
    case Failure(e) => s"Failure($e)"
  }

  override def equals(that: scala.Any): Boolean = (this, that) match {
    case (Success(v1), c: Success[A]) => v1 == c.get
    case (Failure(e), c: Failure) => e == c.msg
    case _ => false
  }
}

case class Success[+A](get: A) extends Result[A]
case class Failure(msg: String) extends Result[Nothing]

