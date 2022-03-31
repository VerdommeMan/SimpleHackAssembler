import Tokens.CInstruction.{computations, destinations, jumps}

import scala.util.matching.Regex


object Tokens {
  // regex definitions for concrete classes below
  val labelRgx: Regex = """^\((\w+)\)$""".r
  val AInstructionNrRgx: Regex = """^@(\d+)$""".r
  val AInstructionSymbolRgx: Regex = """^@(\w+)$""".r
  val CInstructionFullRgx: Regex = """^(.+)=(.+);(\w+)$""".r
  val CInstructionNoDestRgx: Regex = """^(.+);(\w+)$""".r
  val CInstructionNoJumpRgx: Regex = """^(.+)=(.+)$""".r
  val CInstructionOnlyCompRgx: Regex = """^(.){1,3}$""".r

  /**
   * Returns all possible combinations for a given list of characters
   *
   * @param in list of chars
   * @return a sequence of unique combinations
   */
  private def combine(in: List[Char]): Seq[String] =
    for {
      len <- 1 to in.length
      combinations <- in combinations len
    } yield combinations.mkString

  sealed abstract class Hack

  /**
   * There is two types of A Instructions, one which is symbol and one that is value
   * The symbol A instruction holds a symbol representing the value from either predefined symbol table
   * or later on added through the label instruction or variable
   * Is in the form of @value
   *
   * @param value  holds the value next the @token
   * @param symbol is it a symbol or an number
   * @example
   * - @1245
   * - @R2
   * - @_temp
   * - @i
   */
  case class AInstruction(value: String, symbol: Boolean) extends Hack

  object AInstruction {
    def unapply(iL: InstructionLine): Option[AInstruction] = {
      iL.line match {
        case AInstructionNrRgx(number) => Some(AInstruction(number, false))
        case AInstructionSymbolRgx(number) => Some(AInstruction(number, true))
        case _ => None
      }
    }
  }

  /**
   * The C instruction consisting of dest = comp; jump where dest and jump are optional
   *
   * @param dest includes: NULL, A, D, M, MD, AM, AD, AMD
   * @param comp includes: 0, 1, -1, D, A, !D, !A, -D, -A, D+1, A+1, D-1, A-1, D+A, D-A, A-D, D&A, D|A, M, !M, -M, M+1, M-1, D+M, D-M, M-D, D&M, D|M
   * @param jump includes: NULL, JGT, JEQ, JLT, JGE, JNE, JLE, JMP
   * @example
   * M = D-1
   * D-1; JEQ
   * 0;JMP
   * M=D+M
   */
  case class CInstruction(lineNr: Int, dest: String, comp: String, jump: String) extends Hack {
    val isDestValid: Boolean = destinations.contains(dest.toUpperCase)
    val isCompValid: Boolean = computations.contains(comp.toUpperCase)
    val isJumpValid: Boolean = jumps.contains(jump.toUpperCase)
    val isValid: Boolean = isCompValid && isDestValid && isJumpValid

    // Don't like this but dont see a better way
    val errorMsg: String = {
      (if (isValid) "" else s"Line $lineNr: This is an invalid C instruction\n")
        .concat(if (isDestValid) "" else s"\tThis destination is not valid: $dest\n")
        .concat(if (isCompValid) "" else s"\tThis computation is not valid: $comp\n")
        .concat(if (isJumpValid) "" else s"\tThis jump is not valid: $jump\n")
    }

  }

  // Could make it that it only matches valid C instructions but decided not to
  // This way i can improve the error handling
  object CInstruction {
    def unapply(iL: InstructionLine): Option[CInstruction] = {
      iL.line.replaceAll("\\s", "") match {
        case CInstructionFullRgx(dest, comp, jump) => Some(CInstruction(iL.lineNr, dest, comp, jump))
        case CInstructionNoDestRgx(comp, jump) => Some(CInstruction(iL.lineNr, "", comp, jump))
        case CInstructionNoJumpRgx(dest, comp) => Some(CInstruction(iL.lineNr, dest, comp, ""))
        case CInstructionOnlyCompRgx(comp) => Some(CInstruction(iL.lineNr, "", comp, ""))
        case _ => None
      }
    }

    val destinations: Vector[String] = Vector("NULL", "") ++ combine(List('A', 'D', 'M')).toVector
    val jumps: Vector[String] = Vector("NULL", "", "JGT", "JEQ", "JLT", "JGE", "JNE", "JLE", "JMP")
    val computations: Vector[String] = Vector("0", "1", "-1", "D", "A", "!D", "!A", "-D", "-A", "D+1", "A+1", "D-1", "A-1", "D+A", "D-A", "A-D", "D&A", "D|A", "M", "!M", "-M", "M+1", "M-1", "D+M", "D-M", "M-D", "D&M", "D|M")
  }

  /**
   * Holds the label definition
   *
   * @param identifier the label definition
   * @example
   * (LOOP_START)
   * (END)
   * (loop)
   */
  case class Label(lineNr: Int, identifier: String) extends Hack {
    def duplicateDefinitionErrorMsg: String = s"Line $lineNr: Found duplicate label definition ($identifier)\n"

    def existingSymbolErrorMsg: String = s"Line $lineNr: Label definition can not be an existing symbol, for label: $identifier\n"
  }

  object Label {
    def unapply(iL: InstructionLine): Option[Label] = {
      iL.line match {
        case Tokens.labelRgx(text) => Some(Tokens.Label(iL.lineNr, text))
        case _ => None
      }
    }
  }

  /**
   * When a instruction doesnt meet one of the above criteria,
   * the lexer generates this token for error handling
   *
   * @param lineNr the line where this was found
   * @param line   the line which caused it
   */
  case class Unknown(lineNr: Int, line: String) extends Hack {
    val errorMsg: String = s"Line $lineNr: Found unknown instruction ($line)\n"
  }

  /**
   * Holds meta information about the current line
   * @param lineNr the line number where the line was read from the original input
   * @param line the line that was read (stripped and removed comments)
   */
  case class InstructionLine(lineNr: Int, line: String)
}
