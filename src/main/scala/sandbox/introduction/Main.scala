package sandbox.introduction

import cats._
import cats.implicits._

object Main extends App {

  import sandbox.introduction.PrintableInstances._
  import sandbox.introduction.PrintableSyntax._

  val cat = Cat("Tabby", 2, "orange")

  // Using Printable
  Printable.print(cat)
  cat.print

  // Using Show in Cats without Printable
  implicit val catShow: Show[Cat] =
    Show.show(cat => {
      val name = cat.name.show
      val age = cat.age.show
      val color = cat.color.show
      s"$name is a $age year-old $color cat."
    })

  println(cat.show)

  // Using Eq to define equality for Cat
  implicit val catEq: Eq[Cat] =
    Eq.instance[Cat] { (cat1, cat2) =>
      cat1.name === cat2.name &&
      cat1.age === cat2.age &&
      cat1.color === cat2.color
    }

  val cat1 = Cat("Garfield", 38, "orange and black")
  val cat2 = Cat("Heathcliff", 33, "orange and black")
  println(cat1 === cat2)
  println(cat1 =!= cat2)

  val optionCat1 = Option(cat1)
  val optionCat2 = Option.empty[Cat]
  println(optionCat1 === optionCat2)
  println(optionCat1 =!= optionCat2)

}
