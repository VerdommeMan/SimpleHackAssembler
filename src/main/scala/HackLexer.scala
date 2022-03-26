import scala.io.BufferedSource
import scala.util.matching.Regex

object HackLexer {
  private val rgxComment: Regex = """(.*?)//.*""".r

  private def stripComment(line: String): String = {
    line match {
      case rgxComment(text) => text
      case _ => line
    }
  }

  def tokenize(input: BufferedSource): Iterator[Tokens.Hack] = {
    input.getLines()
      .map(stripComment)
      .map(_.strip)
      .zipWithIndex // line counter
      .filterNot(_._1.isBlank) // remove empty lines
      .map {
        case (line, lineCount) => line match {
          case Tokens.Label(instr) => instr
          case Tokens.AInstruction(instr) => instr
          case Tokens.CInstruction(instr) => instr
          case _ => Tokens.Unknown(lineCount + 1, line)

        }
      } ++ Iterator(Tokens.EOI())
  }
}
