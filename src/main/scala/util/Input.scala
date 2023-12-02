package util

import scala.io.Source

object Input {
  // noinspection SourceNotClosed
  def read(fileName: String): LazyList[String] = Source
    .fromFile(s"src/main/resources/$fileName")
    .getLines()
    .map(_.trim)
    .to(LazyList)
}
