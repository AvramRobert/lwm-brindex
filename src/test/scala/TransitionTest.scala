import f.Monoid
import trans.{Failure, Success}
import trans.Transition._

class TransitionTest extends SimpleUnitSpec {

  def stringTransition: String -> String = s => Success(s)

  def stringFailedTransition: String -> String = s => Failure("Transition not possible")

  def stringIntTransition: String -> Int = s => Success(s.toInt)

  def intStringTransition: Int -> String = i => Success(i.toString)

  def listIntTransition: Int -> List[Int] = i => Success((0 to i).toList)

  def sumMonoid = new Monoid[Int] {
    override def append(v1: Int, v2: => Int): Int = v1 + v2

    override def zero: Int = 0
  }

  def concatMonoid = new Monoid[String] {
    override def append(v1: String, v2: => String): String = v1 + v2

    override def zero: String = ""
  }

  "A transition" when {

    "composed" should {

      "maintain associativity, in parametric appropriate conditions, regardless of compositional order" in {
        ((stringTransition ~> stringIntTransition) ~> intStringTransition)("1") == (stringTransition ~> (stringIntTransition ~> intStringTransition))("1")
      }

      "maintain associativity regardless of operation order" in {
        (stringTransition.map(_ + 2) ~> stringIntTransition)("1") == (stringTransition ~> stringIntTransition).map(_ + 2)("1")
      }
    }

    "equated with more than one type of operation" should {

      "maintain its left-biased characteristic" in {

        (stringTransition & (stringIntTransition ~> intStringTransition))("1") == ((stringTransition & stringIntTransition) ~> intStringTransition)("1")
        ((stringTransition & stringIntTransition) ~> intStringTransition)("1") == (stringTransition & stringIntTransition ~> intStringTransition)("1")
        (stringTransition & (stringIntTransition ~> intStringTransition))("1") == (stringTransition & stringIntTransition ~> intStringTransition)("1")
      }
    }

    "being evaluated" should {

      "allow maps" in {
        stringTransition.map(_ + 2)("1") == Success("12")
      }

      "allow flatMaps" in {
        stringTransition.flatMap(s => stringIntTransition)("1") == Success(1)
      }

      "allow folds" in {
        listIntTransition.fold(List(-3, -2, -1))(_ ::: _)(4) == Success(List(-3, -2, -1, 0, 1, 2, 3, 4))
      }

      "allow bidirectional folds" in {
        stringTransition.foldLeft("Hello")(_ + _)("World") == stringTransition.foldRight("World")(concatMonoid.append)("Hello")
      }

      "allow foldMaps" in {
        listIntTransition.foldMap(_.toString())(concatMonoid)(4) == Success("01234")
      }

      "allow non-strict AND" in {
        (stringFailedTransition & stringTransition)("Hello") == Failure("Transition not possible")
      }

      "allow products" in {
        (stringTransition ** stringIntTransition)("1") == Success(("1", 1))
      }

      "allow compositions" in {
        (stringTransition ~> stringIntTransition)("1") == Success(1)
      }
    }
  }


}
