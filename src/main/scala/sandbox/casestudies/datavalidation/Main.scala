package sandbox.casestudies.datavalidation

import cats.data.Validated
import cats.implicits._

object Main extends App {
  sealed trait Check[E, A, B] {
    def apply(a: A): Validated[E, B] =
      ???
    
    def map[C](func: B => C): Check[E, A, C] =
      ???
  }

  val single1 = Single[Vector[String], Int](
    value => if (value == 6) Vector("err1").invalid else value.valid
  )
  val single2 = Single[Vector[String], Int](
    value => if (value % 2 == 0) Vector("err2").invalid else value.valid
  )

  println((single1 and single2).apply(2))
  println((single1 or single2).apply(2))
}
