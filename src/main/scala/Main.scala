import java.io.File
import scala.sys.exit
import scala.io.Source

object Main extends App {
  if (args.length == 0) {
    println("need one arg")
    exit(1)
  }
  val filename = args(0)

  val parser = new HackParser(HackLexer.tokenize(Source.fromFile(filename)))

  parser.parseTokens(new File("filename.hack"))

}
