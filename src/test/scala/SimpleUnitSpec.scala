import org.scalatest.concurrent.Futures
import org.scalatest.{WordSpec, OptionValues, Matchers}

abstract class SimpleUnitSpec extends WordSpec with Matchers with OptionValues with Futures
