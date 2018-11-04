package org.reactive.scala.map

import org.scalatest.{FlatSpecLike, Matchers}

case class UserFilter(name:String, age:Int)

class FilterExercises extends FlatSpecLike with Matchers{
  val listToUse = List(UserFilter("Juan", 53), UserFilter("María", 28), UserFilter("Andrés", 25))

  "The student" should "get only people with more than 40 years" in {
    val result = listToUse.filter(_.age > 40)

    result shouldBe List(UserFilter("Juan", 53))
  }

  it should "delete all the lambdas that are false with the value 5" in {
    val l: List[Int => Boolean] = (1 to 100).toList.map(v => { (x: Int) => x % v == 0 })

    l.filter(f => f(5) == true).length shouldBe 2
    l.filter(_(5)).length shouldBe 2
  }
}
