import commands.{Command, CommandLineParser}
import trans.{Failure, Success}

class CommadLineParserTest extends SimpleUnitSpec {

  val input = "-r Hello -d World"
  val quoted = "-r \"Hello\" -d \"World\""
  val failure = "-r Hello -d"

  val sp1 = "-r Hello World -d No"
  val sp2 = "-r Hello World I am robert -d No"
  val sp3 = ""

  val fp1 = "-r -d No"
  val fp2 = "-d"


  "A command line parser" when {

    "parsing input" should {

      "distinguish parameter and value" in {
        CommandLineParser.parse(input) == Success(Vector(Command("r", "Hello"), Command("d", "World")))
      }

      "only accept key-value pairs" in {
          CommandLineParser.parse(failure) should be (Failure("Insufficient keys or values provided. Please provide key-value pairs."))
      }

      "allow quoted values" in {
        CommandLineParser.parse(quoted) should be (Success(Vector(Command("r", "Hello"), Command("d", "World"))))
      }

      "allow all of these possible input permutations" in {
        CommandLineParser.parse(sp1) should be (Success(Vector(Command("r", "Hello World"), Command("d", "No"))))
        CommandLineParser.parse(sp2) should be (Success(Vector(Command("r", "Hello World I am robert"), Command("d", "No"))))
        CommandLineParser.parse(sp3) should be (Success(Vector()))
      }

      "not allow the following input permutations" in {
        CommandLineParser.parse(fp1) should be (Failure("Insufficient keys or values provided. Please provide key-value pairs."))
        CommandLineParser.parse(fp2) should be (Failure("Insufficient keys or values provided. Please provide key-value pairs."))
      }
    }
  }
}
