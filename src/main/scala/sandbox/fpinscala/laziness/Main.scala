package sandbox.fpinscala.laziness

object Main {
  def main(args: Array[String]): Unit = {
    val stream = Stream(1, 2, 3)
    println(stream.toList)
    println(stream.take(2).toList)
    println(stream.drop(2).toList)
    println(stream.headOption)
    println((stream append stream).toList)
    println(stream.map(_ + 100).toList)
    println((stream zipAll stream.append(stream)).toList)
    println(stream startsWith Empty)
    println(stream startsWith stream.append(stream))
    println(stream.tails.map(_.toList).toList)
    println(stream.scanRight(0)(_ + _).toList)
  }

}
