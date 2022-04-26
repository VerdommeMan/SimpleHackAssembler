import com.monovore.decline.CommandApp
import cats.implicits._

import java.io.File
import scala.io.Source
import com.monovore.decline._

import java.nio.file.Path

object Main extends CommandApp(
  name = "hack-assembler",
  header = "This program is an assembler for the Hack assembly",
  main = {
    val startTime = System.currentTimeMillis()

    val inputFileOpt = Opts.argument[Path](metavar = "path/to/assemblyfile")
    val outputFileOpt = Opts.option[String]("output", short = "o", help = "Specify the output path, if not present it will be put in current directory", metavar = "path/to/outputfile.hack")
    val verboseOpt = Opts.flag("verbose", help = "Whether to be verbose, prints input and output file and duration").orFalse
    val forceOpt = Opts.flag("force", help = "Overwrites output file if it already exists.").orFalse

    (inputFileOpt, outputFileOpt.orNone, verboseOpt, forceOpt).mapN {
      (input, optOutput, verbose, force) => {
        optOutput match {
          case Some(output) => Config(input.toFile, new File(output), verbose, force)
          case None => Config(input.toFile, verbose, force)
        }
      } match {
        case c: Config =>
          c.checkValid()
          val tokenStream = HackLexer.tokenize(Source.fromFile(c.inputFile))
          val parser = new HackParser(tokenStream)
          parser.parseTokens(c.outputFile)
          if (c.verbose) {
            println(
              s"""
                 |The instructions were read from this file: ${c.inputFile.getCanonicalPath}
                 |And were output to the following file: ${c.outputFile.getCanonicalPath}
                 |
                 |This operation was done in ${System.currentTimeMillis() - startTime} ms
                 |""".stripMargin)
          }
      }
    }
  },
  helpFlag = true,
  version = "0.1.0"
)