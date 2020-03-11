package sandbox.introduction

object Main extends App {

  import sandbox.introduction.PrintableInstances._
  import sandbox.introduction.PrintableSyntax._

  val cat = Cat("Tabby", 2, "orange")
  Printable.print(cat)
  cat.print
}
