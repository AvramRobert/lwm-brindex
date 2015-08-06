
import commands.{Command, CommandLineApp}
import trans.Transition._
import indexer._
import BranchIndexer._
import f.ConverterLike._
object Main extends CommandLineApp {

  override def commandSyntax: Map[String, Set[String]] = Map(
    ("add", Set("r", "c", "task", "status")),
    ("list", Set()),
    ("find", Set("p")),
    ("edit", Set("index", "task", "status"))
  )

  override def run: (String) => Transition[Command, _] = {
    case "add" => syntaxCheck() & code() ~> json() ~> postJson("http://192.168.0.100:9000/indexer") ~> fromJson()
    case "list" => syntaxCheck() & get("http://192.168.0.100:9000/cmd/indexer") ~> fromJson()
    case "find" => syntaxCheck() & find() ~> get("http://192.168.0.100:9000/cmd/indexer/find") ~> fromJson()
    case "edit" => syntaxCheck() & edit() ~> json() ~> putJson("http://192.168.0.100:9000/indexer") ~> fromJson()
    case _ => failed("Unknown action")
  }
}
