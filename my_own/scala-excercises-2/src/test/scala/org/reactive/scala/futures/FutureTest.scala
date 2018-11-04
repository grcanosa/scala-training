package org.reactive.scala.futures

import org.scalatest.{FlatSpecLike, Matchers}

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

class FutureTest extends FlatSpecLike with Matchers{
  "Futures" should "be asyncronus executions" in {
    val a = Future{Thread.sleep(5.seconds.toMillis);println("done!");5}
    println("i'm still executing")
    Await.result(a,10.seconds) shouldBe 5
  }

  they should "allow to compose" in {
    val a = Future{10}.map{_ + 5}.map(_.toString).map(_.length)

    Await.result(a,5.seconds) shouldBe 2
  }

  they should "not follow referential transparency" in {
    val futureA: Future[Int] = Future{Thread.sleep(5.seconds.toMillis); 10}
    val futureB: Future[Int] = Future{Thread.sleep(5.seconds.toMillis); 10}

    val futureAplusB = for {
      a <- futureA
      b <- futureB
    } yield a + b

    Await.result(futureAplusB,6.seconds) shouldBe 20


    val invalidFutureAplusB = for {
      a <- Future{Thread.sleep(5.seconds.toMillis); 10}
      b <- Future{Thread.sleep(5.seconds.toMillis); 10}
    } yield a + b

    assertThrows[Throwable] {
      Await.result(invalidFutureAplusB, 6.seconds)
    }

    //Why???
  }

  they should "cache the value once executed" in {
    val future = Future{Thread.sleep(5.seconds.toMillis); 10}
    Await.result(future, 6.seconds) shouldBe 10
    Await.result(future, 1.second)  shouldBe 10
  }
}
