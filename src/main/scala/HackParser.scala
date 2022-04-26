import HackParser.{REGISTER_OFFSET, symbolTable}

import java.io.{File, PrintWriter}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class HackParser(tokens: Iterator[Tokens.Hack]){
  var instructionCounter = 0
  var variableCounter = 0 // tracks how many variables are instantiated
  // this keeps track of the defined labels and variables, preloaded with keywords
  val fullSymbolTable: mutable.Map[String, Int] = mutable.Map[String, Int]() ++= symbolTable
  val errors: ListBuffer[String] = mutable.ListBuffer[String]()

  /**
   * Uses two passes, first pass adds label definitions and checks for errors
   * Second pass translates the instructions and writes to the file
   *
   * @param outFile the file were we are writing to
   */
  def parseTokens(outFile: File): Unit = {
    val (pass1, pass2) = tokens.duplicate

    // pass 1, find errors and label definitions
    pass1.foreach {
      case t: Tokens.Unknown => errors.addOne(t.errorMsg)
      case c: Tokens.CInstruction if !c.isValid => errors.addOne(c.errorMsg)
      case _: Tokens.AInstruction | _: Tokens.CInstruction => instructionCounter += 1
      case l: Tokens.Label => addLabelDefinition(l)
      case _ => throw new UnsupportedOperationException
    }

    // Abort procedure when errors are found
    if (handleErrors) return
    val fileWriter: PrintWriter = new PrintWriter(outFile)

    // second phase: translate tokens
    pass2
      .filterNot(_.isInstanceOf[Tokens.Label])
      .map{
        case c: Tokens.CInstruction => c.toBinary
        case a: Tokens.AInstruction => toBinary(a)
      }
      .foreach(fileWriter.println)
  }

  def toBinary(instr: Tokens.AInstruction): String = {
    val value: Int = if (instr.isSymbol) {
      fullSymbolTable.getOrElseUpdate(instr.value, {
        variableCounter += 1
        variableCounter - 1 + REGISTER_OFFSET
      })
    } else instr.value.toInt
    value.toBinaryString.takeRight(15).reverse.padTo(16, '0').reverse // lefts pads zeros until 16 length
  }

  def addLabelDefinition(label: Tokens.Label): Unit = {
    if (symbolTable.contains(label.identifier))
      errors.addOne(label.existingSymbolErrorMsg)
    else if (fullSymbolTable.contains(label.identifier))
      errors.addOne(label.duplicateDefinitionErrorMsg)
    else
      fullSymbolTable(label.identifier) = instructionCounter
  }

  def handleErrors: Boolean = {
    if (errors.isEmpty) return false
    System.err.println(s"Process aborted! Found ${errors.length} errors, fix these to proceed\n")
    errors.foreach(System.err.print)
    true
  }
}

object HackParser {
  // Holds keywords
  val symbolTable: Map[String, Int] = Map(
    "SP" -> 0,
    "LCL" -> 1,
    "ARG" -> 2,
    "THIS" -> 3,
    "THAT" -> 4,
    "R0" -> 0,
    "R1" -> 1,
    "R2" -> 2,
    "R3" -> 3,
    "R4" -> 4,
    "R5" -> 5,
    "R6" -> 6,
    "R7" -> 7,
    "R8" -> 8,
    "R9" -> 9,
    "R10" -> 10,
    "R11" -> 11,
    "R12" -> 12,
    "R13" -> 13,
    "R14" -> 14,
    "R15" -> 15,
    "SCREEN" -> 16384,
    "KBD" -> 24576
  )

  // the first 16 address locations are reserved for register
  val REGISTER_OFFSET = 16
}
