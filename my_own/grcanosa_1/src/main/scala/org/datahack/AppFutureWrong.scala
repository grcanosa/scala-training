package org.datahack
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success}

object AppFutureWrong {
  def main(args: Array[String]): Unit = {
    println(System.currentTimeMillis())
    val f1 = Future{Thread.sleep(3.seconds.toMillis); println(s"f1 ${System.currentTimeMillis()}")}
    val f2 = Future{Thread.sleep(3.seconds.toMillis); println(s"f2 ${System.currentTimeMillis()}")}

    for {
      f1r <- f1
      f2r <- f2
    } yield println(s"yield1 ${System.currentTimeMillis()}")

    //NO es lo mismo que hacer:

    for {
      f1r <- Future{Thread.sleep(3.seconds.toMillis); println(s"f1.2 ${System.currentTimeMillis()}")}
      f2r <- Future{Thread.sleep(3.seconds.toMillis); println(s"f2.2 ${System.currentTimeMillis()}")}
    } yield println(s"yield2 ${System.currentTimeMillis()}")



    Thread.sleep(8.seconds.toMillis)
  }
}
