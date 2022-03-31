import jdk.jshell.spi.ExecutionControl.NotImplementedException

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object HackParser {
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

  /**
   * Uses two passes, first pass adds label definitions and checks for errors
   *
   * @param tokens
   * @param outFile
   */
  def parseTokens(tokens: Iterator[Tokens.Hack], outFile: String): Unit = {
    var instructionCounter = 0
    val labelDefinitions = mutable.Map[String, Int]()
    val errors = mutable.ListBuffer[String]()

    // pass 1, find errors and label definitions
    tokens.foreach {
      case t: Tokens.Unknown => errors.addOne(t.errorMsg)
      case c: Tokens.CInstruction if !c.isValid => errors.addOne(c.errorMsg)
      case _: Tokens.AInstruction | _: Tokens.CInstruction  => instructionCounter += 1
      case l: Tokens.Label => addLabelDefinition(l, instructionCounter, labelDefinitions, errors)
      case _ => throw new UnsupportedOperationException
    }

    // Abort procedure when errors are found
    if (handleErrors(errors.toList)) return

    // second phase: translate tokens

    tokens.foreach {
      case c: Tokens.CInstruction =>
      case a: Tokens.AInstruction =>
    }


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
