package org.reactive.scala

import org.scalatest.{FlatSpecLike, Matchers}

class Recursion extends FlatSpecLike with Matchers {
  "The student" should "implement fibonacci sucesion" in {
    def fib(n:Int):Int = n match {
      case 0 => 0
      case 1 => 1
      case n => fib(n-1)+fib(n-2)
    }

    fib(0) shouldBe 0
    fib(1) shouldBe 1
    fib(2) shouldBe 1
    fib(3) shouldBe 2
    fib(4) shouldBe 3
    fib(5) shouldBe 5
  }

  "Tail recursive" should "push the limits of recusion" in {
    //Implement the same function with normal recusion and tail recursion

    def normalVersion(n:Int):Long = n match {
      case 0 => 0
      case n => n + normalVersion(n - 1)
    }
    def tailRecusrive(n:Int):Long ={
      def aux(n:Int, acum:Long):Long = {
        n match {
          case 0 => acum
          case n => aux(n-1,acum+n)
        }
      }
      aux(n,0)
    }

    normalVersion(1000) shouldBe tailRecusrive(1000)

    assertThrows[StackOverflowError]{
      normalVersion(50000)
    }

    tailRecusrive(50000) shouldBe 1250025000
  }
}
