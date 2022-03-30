case class HackError(lineNr: Int, line: String, msg: Option[String]) {
  override def toString: String = msg.getOrElse(s"Unknown instruction on line: $lineNr \n$line\n")
}
