import java.io.File
import scala.sys.exit

case class Config(inputFile: File, outputFile: File, verbose: Boolean = false, force: Boolean = false) {

  /**
   * If it finds an extension then it replaces the extension with .hack
   * else it just appends .hack extension
   *
   * @param inputFile the file where it reads from
   */
  def this(inputFile: File, verbose: Boolean, force: Boolean) = {
    this(inputFile, {
      var filename = inputFile.getName
      val pos = filename.lastIndexOf(".")
      if (pos > 0)
        filename = filename.substring(0, pos)

      new File(filename + ".hack")
    }, verbose, force)
  }

  def checkValid(): Unit = {
    if (!(inputFile.exists() && inputFile.isFile && inputFile.canRead)) {
      System.err.println("Input file does not exist, or is not a file or can not be read")
      exit(1)
    }

    if (!(!outputFile.exists() || force)) {
      System.err.println("Output file already exists, use -force to overwrite said file")
      exit(1)
    }
  }
}

object Config {
  def apply(inputFile: File, verbose: Boolean, force: Boolean) = new Config(inputFile, verbose, force)
}