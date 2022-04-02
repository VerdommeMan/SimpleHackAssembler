import Tokens.CInstruction.{computations, destinations, jumps}

import scala.language.implicitConversions
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
   * @param value    holds the value next the @token
   * @param isSymbol is it a symbol or an number
   * @example
   * - @1245
   * - @R2
   * - @_temp
   * - @i
   */
  case class AInstruction(value: String, isSymbol: Boolean) extends Hack

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

    /**
     * C instruction 16 bit(see README specification)
     * 1xxA cccc ccdd djjj
     * 1 is the opcode, there is only two opcodes 0 and 1, for A instruction and C instruction
     * x bits arent used, A bit decides if it should retrieve from reg A (0) or M[A] (1)
     * c bits are the comp bits used by the alu, j bits represents the jump destination
     * d bits are the destination, where it should store the result (A, D, M)
     *
     * @note
     * Uses an implicit to convert booleans to string [[CInstruction.bool2str]]
     * @return C instruction as 16 bit binary string
     */

    def toBinary: String =
      "111" + computations(comp) +
        bool2str(dest.contains('A')) +
        bool2str(dest.contains('D')) +
        bool2str(dest.contains('M')) +
        jumps(jump)

    private def bool2str(b: Boolean): String = if (b) "1" else "0" // converts booleans to string "1" and "0"

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
    val jumps: Map[String, String] = Map( // 3 bits
      ("NULL", "000"),
      ("", "000"),
      ("JGT", "001"),
      ("JEQ", "010"),
      ("JGE", "011"),
      ("JLT", "100"),
      ("JNE", "101"),
      ("JLE", "110"),
      ("JMP", "111")
    )
    val computations: Map[String, String] = Map( // 7 bits
      "0" -> "0101010",
      "1" -> "0111111",
      "-1" -> "0111010",
      "D" -> "0001100",
      "A" -> "0110000",
      "!D" -> "0001101",
      "!A" -> "0110001",
      "-D" -> "0001111",
      "-A" -> "0110011",
      "D+1" -> "0011111",
      "A+1" -> "0110111",
      "D-1" -> "0001110",
      "A-1" -> "0110010",
      "D+A" -> "0000010",
      "D-A" -> "0010011",
      "A-D" -> "0000111",
      "D&A" -> "0000000",
      "D|A" -> "0010101",
      "M" -> "1110000",
      "!M" -> "1110001",
      "-M" -> "1110011",
      "M+1" -> "1110111",
      "M-1" -> "1110010",
      "D+M" -> "1000010",
      "D-M" -> "1010011",
      "M-D" -> "1000111",
      "D&M" -> "1000000",
      "D|M" -> "1010101")
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
   *
   * @param lineNr the line number where the line was read from the original input
   * @param line   the line that was read (stripped and removed comments)
   */
  case class InstructionLine(lineNr: Int, line: String)
}
