import java.io.File
import scala.sys.exit
import scala.io.Source

object Main extends App {
  if (args.length == 0) {
    println("need one arg")
    exit(1)
  }
  val filename = args(0)
  HackParser.parseTokens(HackLexer.tokenize(Source.fromFile(filename)), new File("filename.hack"))

}
