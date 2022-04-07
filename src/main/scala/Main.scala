import java.io.File
import scala.sys.exit
import scala.io.Source

object Main extends App {
  val helpMsg =
    """
      |This program is an assembler for the Hack assembly.
      |
      |USAGE:
      | hackassembler path/to/input.asm [path/to/output.hack] [-verbose] [-force]
      |
      |FLAGS:
      |   verbose: prints success and file written to
      |   force: overwrite if output file already exists
      |
      |""".stripMargin

  if (args.length == 0) {
    println(helpMsg)
    exit(1)
  }

  val config: Config = args.length match {
    case 1 => Config(new File(args(0)))
    case 2 => Config(new File(args(0)), new File(args(1)))
    case 3 => Config(new File(args(0)), new File(args(1)), true)
    case 4 => Config(new File(args(0)), new File(args(1)), true)
    case _ => System.err.println("Wrong amount of arguments given!"); exit(1)
  }

  val parser = new HackParser(HackLexer.tokenize(Source.fromFile(filename)))

  parser.parseTokens(new File("filename.hack"))

}
