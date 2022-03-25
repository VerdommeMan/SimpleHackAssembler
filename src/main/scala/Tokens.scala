import scala.util.matching.Regex


object Tokens {
  // regex definitions for concrete classes below
  val labelRgx: Regex = """^\((\w+)\)\s*$""".r


  sealed abstract class Hack

  /**
   * There is two types of A Instructions, one which is symbol and one that is value
   * The symbol A instruction holds a symbol representing the value from either predefined symbol table
   * or later on added through the label instruction or variable
   * Is in the form of @value
   *
   * @param value holds the value next the @token
   * @param symbol is it a symbol or an number
   *
   * @example
   * - @1245
   * - @R2
   * - @_temp
   * - @i
   */
  case class AInstruction(value:String, symbol: Boolean) extends Hack

  /**
   * The C instruction consisting of dest = comp; jump where dest and jump are optional
   *
   *
   * @param dest includes: null, A, D, M, MD, AM, AD, AMD
   * @param comp includes: 0, 1, -1, D, A, !D, !A, -D, -A, D+1, A+1, D-1, A-1, D+A, D-A, A-D, D&A, D|A, M, !M, -M, M+1, M-1, D+M, D-M, M-D, D&M, D|M
   * @param jump includes: null, JGT, JEQ, JLT, JGE, JNE, JLE, JMP
   *
   * @example
   * M = D-1
   * D-1; JEQ
   * 0;JMP
   * M=D+M
   */
  case class CInstruction(dest: String, comp: String, jump: String) extends Hack

  /**
   * Holds the label definition
   *
   * @param label the label definition
   *
   * @example
   * (LOOP_START)
   * (END)
   * (loop)
   */
  case class Label(label: String) extends Hack

  /**
   * When a instruction doesnt meet one of the above criteria,
   * the lexer generates this token for error handling
   * @param lineNr the line where this was found
   * @param line the line which caused it
   */
  case class Unknown(lineNr: Int, line: String) extends Hack

  /**
   * End Of Input, when the lexer hits the end of the input it generates this token
   */
  case class EOI() extends Hack
}
