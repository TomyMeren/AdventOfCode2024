object Utils {
  def openFile(path: String): List[String] = {
    val source = scala.io.Source.fromFile(path)
    val lines = source.getLines().toList
    lines
  }
}

object Part01 {

  def split(l: List[String]): List[(Long, Long)] =
    val ll: List[Array[Long]] = l.map(s => s.split("   ").map(_.toLong))
    ll.map(_.head).sorted.zip(ll.map(_.last).sorted)

  def calculate(l: List[(Long, Long)]): Long =
    l.foldLeft(0L) { case (acc, (a, b)) =>
      acc + math.abs(a - b)
    }
}

object Part02 {
  def calculate2(l: List[(Long, Long)]): Long =
    val rightPart = l.map(_._2)

    l.foldLeft(0L) { case (acc, (a, b)) =>
      acc + a * rightPart.filter(a == _).length
    }
}

@main def Main(): Unit = {
  import Part01.*
  import Part02.*

  val lines = Utils.openFile("01/01.txt")
  val splitedLines = split(lines)
  println(calculate(splitedLines))
  println(calculate2(splitedLines))
}
