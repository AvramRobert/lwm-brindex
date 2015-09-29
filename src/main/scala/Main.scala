
import commands.{Command, CommandLineApp}
import trans.Transition._
import indexer._
import BranchIndexer._
import f.ConverterLike._
object Main extends CommandLineApp {

  override def commandSyntax: Map[String, Set[String]] = Map(
    ("add", Set("r", "c", "task", "status", "card")),
    ("list", Set()),
    ("help", Set()),
    ("wiki", Set()),
    ("find", Set("p")),
    ("edit", Set("index", "task", "status", "card"))
  )

  override def run: (String) => Command -> _ = {
    case "add" => syntaxCheck() & code() ~> json() ~> postJson("https://praktikum.gm.fh-koeln.de/indexer") ~> fromJson()
    case "list" => syntaxCheck() & get("https://praktikum.gm.fh-koeln.de/cmd/indexer") ~> fromJson()
    case "find" => syntaxCheck() & find() ~> get("https://praktikum.gm.fh-koeln.de/cmd/indexer/find") ~> fromJson()
    case "edit" => syntaxCheck() & edit() ~> json() ~> putJson("https://praktikum.gm.fh-koeln.de/indexer") ~> fromJson()
    case "help" => syntaxCheck() & fromFile("res/help.txt")
    case "wiki" => syntaxCheck() & fromFile("res/wiki.txt")
    case _ => failed("Unknown action")
  }
}
