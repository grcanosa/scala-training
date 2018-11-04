package org.reactive.scala.map

import org.scalatest.{FlatSpecLike, Matchers}

class FlatmapExercises extends FlatSpecLike with Matchers{
  "Flatmap" should "do the same as a map and flatten" in {
    List(1,2,3).flatMap(1 to _) shouldBe List(1,2,3).map(1 to _).flatten
  }

  it should "work with multiple data structures" in {
    Option(10).flatMap(v => if (v % 2 == 0) None else Some(v)) shouldBe None
  }

  it should "work with mixed data structures" in {
    List(1,2,3).flatMap(n => (1 to n).toVector) shouldBe List(1,1,2,1,2,3)
  }

  "The student" should "create his own filter method" in {
    //create a 'filter' method that keeps the elements  that satify the condition
    //for any type of list

    def filter[A](list:List[A], condition:A => Boolean) = {
      //list.filter(condition)
      list.flatMap(e => if(condition(e)) Option(e) else None)
    }

    filter[Int](List(1,2,3,4), a => a % 2 == 0)
  }

  it should "see that for comrehendion is great" in {
    val numberList = List(1,2,3)
    val letterList = List("a","b","c")

    val flatmapVersion = numberList.flatMap(number => letterList.map(letter => (number, letter)))
    val forVersion = for {
      number <- numberList
      letter <- letterList
    } yield (number, letter)

    flatmapVersion shouldBe forVersion
    flatmapVersion shouldBe List((1,"a"),(1,"b"),(1,"c"),(2,"a"),(2,"b"),(2,"c"),(3,"a"),(3,"b"),(3,"c"))
  }

  it should "do this complex code, with for a without it" in {
    //create all combinations of each list except if the sum of the numbers is even
    //and if the letter is b it doesn't admit any odd number
    val list1 = (1 to 5).toList
    val list2 = (1 to 3).toList
    val list3 = ('a' to 'b').toList

    val resultList = for {
      n1 <- list1
      n2 <- list2
      if ((n1 + n2) % 2) == 0
      letter <- list3
      if letter == 'b' && ((n1 % 2 == 1) || (n2 % 2 == 1))
    } yield (n1,n2,letter)

    val resultListFlat = list1.flatMap(n1 => list2.flatMap(n2 => list3.map(letter => (n1,n2,letter)))).filter(t => {
      ((t._1+t._2) %2 == 0) && (t._3 == 'b' && ((t._1 % 2 == 1) || (t._2 % 2 == 1)))
    })
    resultList shouldBe List((1,1,'b'), (1,3,'b'), (3,1,'b'), (3,3,'b'), (5,1,'b'), (5,3,'b'))
    resultListFlat shouldBe List((1,1,'b'), (1,3,'b'), (3,1,'b'), (3,3,'b'), (5,1,'b'), (5,3,'b'))
  }
}
