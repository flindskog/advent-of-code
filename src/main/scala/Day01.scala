import scala.io.Source

trait Day01 {
  lazy val data =
    Source
      .fromInputStream(getClass.getResourceAsStream("input_01.txt"))
      .getLines()
      .map(_.trim)
      .to(LazyList)
}

object Day01_1 extends App with Day01 {
  val result = data.headOption


  println(result)
}
