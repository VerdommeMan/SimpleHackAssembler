import jdk.jshell.spi.ExecutionControl.NotImplementedException

import java.io.{File, PrintWriter}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object HackParser {
  // Holds keywords
  val symbolTable = Map(
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

  val VARIABLE_OFFSET = 16 // the first 16 address locations are reserved for keywords

  /**
   * Uses two passes, first pass adds label definitions and checks for errors
   *
   * @param tokens
   * @param outFile
   */
  def parseTokens(tokens: Iterator[Tokens.Hack], outFile: File): Unit = {
    var instructionCounter = 0
    var variableCounter = 0 // tracks how many variables are instantiated
    val labelVariableMap = mutable.Map[String, Int]() // this keeps track of the defined labels and variables
    val errors = mutable.ListBuffer[String]()
    val (pass1, pass2) = tokens.duplicate

    // pass 1, find errors and label definitions
    pass1.foreach {
      case t: Tokens.Unknown => errors.addOne(t.errorMsg)
      case c: Tokens.CInstruction if !c.isValid => errors.addOne(c.errorMsg)
      case _: Tokens.AInstruction | _: Tokens.CInstruction => instructionCounter += 1
      case l: Tokens.Label => addLabelDefinition(l, instructionCounter, labelVariableMap, errors)
      case _ => throw new UnsupportedOperationException
    }

    // Abort procedure when errors are found
    if (handleErrors(errors.toList)) return
    val fileWriter: PrintWriter = new PrintWriter(outFile)

    // second phase: translate tokens
    pass2.foreach {
      case c: Tokens.CInstruction => fileWriter.println(c.toBinary)
      case instr: Tokens.AInstruction =>
        val value: Int = if (instr.isSymbol) {
          labelVariableMap.getOrElseUpdate(instr.value, { // if not found means it is instantiated a new variable
            variableCounter += 1
            variableCounter + VARIABLE_OFFSET - 1
          })
        } else instr.value.toInt

        val binary = value.toBinaryString.takeRight(15).reverse.padTo(16, '0').reverse
        fileWriter.println(binary)
      case _ =>
    }
    fileWriter.close()
  }

  def addLabelDefinition(label: Tokens.Label, instrCounter: Int, labelDefinitions: mutable.Map[String, Int], errors: ListBuffer[String]): Unit = {
    if (labelDefinitions.contains(label.identifier)) errors.addOne(label.duplicateDefinitionErrorMsg)
    else if (symbolTable.contains(label.identifier)) errors.addOne(label.existingSymbolErrorMsg)
    else labelDefinitions(label.identifier) = instrCounter
  }

  def handleErrors(errors: List[String]): Boolean = {
    if (errors.isEmpty) return false
    System.err.println(s"Process aborted! Found ${errors.length} errors, fix these to proceed\n")
    errors.foreach(System.err.print)
    true
  }

}
