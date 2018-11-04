package org.datahack

import scala.collection.immutable
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.io.Source
import scala.util.{Failure, Success}

object App3_Futures {
  def main(args:Array[String]):Unit = {
    val number = Future{
      println("empiezo")
      Thread.sleep(2.seconds.toMillis)
      println("termina")
      10
    }
    val leerAs: String => Future[Int] = filename => Future {
      println(s"empiezo a contar Aes en $filename")
      val text = Source.fromFile(filename).getLines
      println(s"termino de contar aes en $filename")
      text.mkString("").count(_ == 'a')
    }

    number.onComplete{
      case Success(value) => println(s"returned $value")
      case scala.util.Failure(exception) => println(s"error ${exception.getMessage}")
    }
    val res = leerAs("/tmp/prueba.txt").transform{   //Transform es como un map pero tanto si ha ido bien como mal.
      case Success(value) => println(s"cuenta: $value"); Success(())
      case Failure(exception) => println(s"error ${exception.getMessage}"); Failure(exception)
    }

    val res2 = leerAs("/tmp/prueba.txt").map(value => s"returned cuenta $value")


    val listOfFutures = (1 to 4).map(i => s"/mnt/DATAEXT/DATAHACK/Scala/code/grcanosa_1/src/main/resources/$i.txt")
          .map(leerAs)

    //countAll es una lista de futuros, nosotros queremos un futuro de una lista
    // esto se hace con Future.sequence
    val futureOfListInt = Future.sequence(listOfFutures)

    val suma = futureOfListInt.map(_.sum) //Ahora ya es un resultado.
                                            // Ya que al hacer map de un Future te da el una lista de un solo valor

    val result = suma.map(value => s"returned value count: $value")

    val result1: Future[immutable.IndexedSeq[Int]] = Future.sequence(listOfFutures)
    val result2: Future[Int] = result1.map(_.sum)
    val result3: Future[String] = result2.map(value => s"returned value $value")



    Await.result(number, 5.seconds)  // ESTO SOLO SE USA PARA TESTING!!!!
    Await.result(res, 2.seconds)
    println(Await.result(res2, 2.seconds))
    println(Await.result(result3, 3.seconds))
    //Thread.sleep(3.seconds.toMillis)
  }
}
