package org.reactive.scala.map

import org.scalatest.{FlatSpecLike, Matchers}

class FoldExercises extends FlatSpecLike with Matchers{
  "The student" should "transform the for loop into a fold" in {
    var acum = 0
    for {
      n <- 1 to 100
    } yield acum = acum + n

    val foldedAcum = (1 to 100).foldLeft(0)((acc,i) => acc + i)
    val foldedAcum2 = (1 to 100).foldLeft(0)(_ + _)

    acum shouldBe foldedAcum
  }

  it should "transfor other for loop" in {
    /*
      for (i in

     */
  }

  it should "do a filter using fold" in {
    val withFilter = (1 to 100).toList.filter(_ % 2 == 0)
    val withFold = (1 to 100).foldLeft(List.empty[Int])((acum, i) => if (i % 2 == 0) i :: acum else acum).reverse

    withFilter shouldBe withFold
  }

  it should "implement reverse with a fold in a lambda" in {
    val studentReverse:List[Int] => List[Int] = _.foldLeft(List.empty[Int])((acc,e  ) => e :: acc)
    val studentReverse2:List[Int] => List[Int] = l => l.foldLeft(List.empty[Int])((acc,e  ) => e :: acc)
    //Con guiones no se puede hacer porque el primer elemento no es el primer _
    //val studentReverse3:List[Int] => List[Int] = l => l.foldLeft(List.empty[Int])(_ :: _)
    val listToTest = (1 to 100).toList

    studentReverse(listToTest) shouldBe listToTest.reverse
  }

  it should "create a single filter with all the given ones ones" in {
    val values = (1 to 1000).toList

    val filter1:Int => Boolean = _ > 10
    val filter2:Int => Boolean = _ % 2 == 0
    val filter3:Int => Boolean = _ < 1000
    val filter4:Int => Boolean = _ % 13 != 0

    //maybe, it will be usefull to create a lambda to join two filters

    val acceptAll:Int => Boolean = _ =>  true

    //MIO
    val supremeFilter:Int => Boolean = List(filter1,filter2,filter3,filter4)
                                      .foldLeft(acceptAll)((fAcc, f) => {
                                              i: Int => fAcc(i) & f(i)
                                      })
    //PROFESOR
    val supremeFilter2:Int => Boolean = List(filter1,filter2,filter3,filter4)
      .foldLeft((i:Int) => true)((fAcc, f) => (i:Int) => f(i) && fAcc(i))

    //Otra opcion seria hacer un filter mixer
    val filterMixer:(Int => Boolean, Int => Boolean) => Int => Boolean = (f1,f2) => (i:Int) => f1(i) && f2(i)

    type fIB = (Int=>Boolean)
    val foldF: (fIB,fIB) => fIB = (f1,f2) => (i:Int) => f1(1) && f2(i)
    val supremeFilter3:Int => Boolean = List(filter1,filter2,filter3,filter4).fold((i:Int) => true)(foldF)

    values.filter(supremeFilter) shouldBe values.filter(filter1).filter(filter2).filter(filter3).filter(filter4)
    values.filter(supremeFilter3) should have size 456

  }
}
