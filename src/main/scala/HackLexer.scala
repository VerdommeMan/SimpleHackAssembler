import Tokens.Label

import java.io.BufferedReader
import scala.util.matching.Regex

object HackLexer {
  private val rgxComment: Regex = """(.*?)\/\/.*""".r

  private def stripComment(line: String): String = {
    line match {
      case rgxComment(text) => text
      case _ => line
    }
  }

  def tokenize(input: BufferedReader): List[Tokens.Hack] = {
    var lineCount = 0
    while (input.ready) {

      stripComment(input.readLine().stripLeading()) match {
        case Tokens.labelRgx(text) => Tokens.Label(text)
        case _ => Tokens.Unknown(lineCount, _)
      }
      lineCount += 1
    }
    List()
  }
}
