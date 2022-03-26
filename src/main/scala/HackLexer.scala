import java.io.BufferedReader
import scala.io.{BufferedSource, Source}
import scala.util.matching.Regex
import scala.jdk.StreamConverters._

object HackLexer {
  private val rgxComment: Regex = """(.*?)//.*""".r

  private def stripComment(line: String): String = {
    line match {
      case rgxComment(text) => text
      case _ => line
    }

  }

  def tokenize(input: BufferedReader): List[Tokens.Hack] = {
    input.lines()
      .toScala(LazyList)
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
      }.toVector.appended(Tokens.EOI()).toList
  }

  def tokenize(input: BufferedSource): Iterator[Tokens.Hack] = {
    input.getLines()
      .map(stripComment)
      .map(_.strip)
      .zipWithIndex // line counter
      .filterNot(_._1.isBlank) // remove empty lines
      .map {
        case (line, lineCount) => line match {
          case Tokens.labelRgx(text) => Tokens.Label(text)
          case Tokens.AInstructionNrRgx(number) => Tokens.AInstruction(number, false)
          case Tokens.AInstructionSymbolRgx(number) => Tokens.AInstruction(number, true)
          case Tokens.CInstruction(instr) => instr
          case _ => Tokens.Unknown(lineCount + 1, line)

        }
      } ++ Iterator(Tokens.EOI())
  }
}
