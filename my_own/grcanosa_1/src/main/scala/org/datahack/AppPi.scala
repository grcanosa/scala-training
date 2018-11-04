package org.datahack



import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random
import scala.concurrent.duration._
import scala.util.{Failure, Success}

object AppPi {

  def times[A](f: => A):A = {
    val t1 = System.currentTimeMillis()
    val a = f
    println("TIME:"+(System.currentTimeMillis()-t1).toString)
    a
  }

  def modo1(npoints:Int,nfutures:Int): Unit ={
    def puntosEnCirculo(n:Int):Int = {
      val points = for {
        _ <- (1 to n).toList
        x = Random.nextFloat()
        y = Random.nextFloat()
      } yield (x,y)
      //println(points)
      val en_circulo:Int = points.map{case (x,y) => (Math.pow(x,2)+ Math.pow(y,2)) <= 1}.count(_ == true)
      en_circulo
    }
    val coin_tosses = npoints
    println("Lanzando las monedas una a una")
    val t1 = System.currentTimeMillis()
    val en_circulo = times(puntosEnCirculo(coin_tosses))
    val pi = en_circulo.toDouble / coin_tosses * 4
    println(pi)
    //Futuros
    println("Lanzando los futuros")
    val coin_tosses_futures = npoints
    val n_futuros = nfutures
    val futuros = List.fill(n_futuros)(coin_tosses_futures/n_futuros).map(c => Future[Int]{puntosEnCirculo(c)})
    val suma: Future[Int] = Future.sequence(futuros).map(_.sum)

    //Await.result(suma,30.seconds)
    println(times(Await.result(suma,30.seconds)).toDouble / coin_tosses_futures * 4)
  }

  def modo11(npoints:Int,nfutures:Int): Unit ={

    //Otra forma de hacer la funcion circulo es con una lambda
    val pEnCir: Int=> Int = n => (1 to n).map(_ => Math.pow(Random.nextFloat(),2) + Math.pow(Random.nextFloat(),2) <= 1).count(_ == true)

    //Mas resumido los futuros
    val coint_t_3 = npoints
    val n_fut_3 = nfutures
    val pi_f = Future.sequence(List.fill(n_fut_3)(Future[Int]{pEnCir(coint_t_3/n_fut_3)})).map(_.sum.toDouble *4 / coint_t_3)
    println(Await.result(pi_f,30.seconds))
  }

  //Métodos comunes a las formas del profesor
  val getRand: () => Float = () => Random.nextFloat()
  val getCoin: () => (Float,Float) = () => (getRand(),getRand())
  val getRand2: Random => Float = r => r.nextFloat()
  val getCoin2: Random => (Float,Float) = r => (getRand2(r),getRand2(r))
  val isInCircle: (Float,Float) => Boolean = (x,y) => pow2(x)+pow2(y) <= 1
  val oneIfTrue: Boolean => Int = if(_) 1 else 0
  val pow2: Float => Float = x => x * x

  val getPiAproximator: Int => Int => Float = x => y => y.toFloat / x * 4

  val getPiAproximator2: Int => (Int => Float) = x => (y => y.toFloat / x * 4)

  def modo2(npoints:Int,nfutures:Int): Unit ={
    //Manera haciendo distintas funciones

    val throwNCoins: Int => Int = n => (1 to n).map(_ => getCoin()).map(isInCircle.tupled).map(oneIfTrue).sum
    val n = npoints

    val piAproximator = getPiAproximator(npoints)
    println(times(piAproximator(throwNCoins(npoints))))
  }

  def modo3(npoints:Int,nfutures:Int): Unit ={

    val throwNCoins: Int => Int = n => (1 to n).map(_ => getCoin()).map(isInCircle.tupled).map(oneIfTrue).sum

    val piAproximator = getPiAproximator(npoints*nfutures)

    val futureList = (1 to nfutures).map(_ => Future{throwNCoins(npoints)})
    val res = Future.sequence(futureList).map(_.sum).map(piAproximator)
    println(times(Await.result(res,30.seconds)))
  }

  def modo4(npoints:Int,nfutures:Int): Unit ={
    //Se ve que con cuatro hilos tarda mas y debería tardar lo mismo
    //Esto es porque el Random es una sola instancia compartida entre otro

    val throwNCoins2: Int => Int = n => {
      val r = new Random()
      (1 to n).map(_ => getCoin2(r)).map(isInCircle.tupled).map(oneIfTrue).sum
    }

    val piAproximator = getPiAproximator(npoints*nfutures)

    val futureList = (1 to nfutures).map(_ => Future{throwNCoins2(npoints)})
    val res = Future.sequence(futureList).map(_.sum).map(piAproximator)
    println(times(Await.result(res,30.seconds)))
  }

  def modo5(npoints:Int,nfutures:Int): Unit ={

    val getIfIsInCircle = getCoin2 andThen isInCircle.tupled andThen oneIfTrue
     //Probar esta version que sería la más rápido!!
    val throwNCoins3: Int => Int = n => {
      val r = new Random()
      (1 to n).map(_ => getIfIsInCircle(r)).sum
    }
    val piAproximator = getPiAproximator(npoints*nfutures)

    val futureList = (1 to nfutures).map(_ => Future{throwNCoins3(npoints)})
    val res = Future.sequence(futureList).map(_.sum).map(piAproximator)
    println(times(Await.result(res,30.seconds)))
  }

  def modo6(npoints:Int,nfutures:Int): Unit ={

    val getIfIsInCircle = getCoin2 andThen isInCircle.tupled andThen oneIfTrue

    //Probar esta version que sería la más rápido!!
    val throwNCoins4: Int => Int = n => {
      val r = new Random()
      (1 to n).foldLeft(0)((acc,_) => acc + getIfIsInCircle(r))
    }

    val piAproximator = getPiAproximator(npoints*nfutures)

    val futureList = (1 to nfutures).map(_ => Future{throwNCoins4(npoints)})
    val res = Future.sequence(futureList).map(_.sum).map(piAproximator)
    println(times(Await.result(res,30.seconds)))
  }


  def main(args: Array[String]): Unit = {
    val npoints=1000000
    val nfutures=8
    println("MODO 1 *******************************************")
    modo1(npoints,nfutures)
    println("MODO 2 *******************************************")
    modo2(npoints,nfutures)
    println("MODO 3 *******************************************")
    modo3(npoints,nfutures)
    println("MODO 4 *******************************************")
    modo4(npoints,nfutures)
    println("MODO 5 *******************************************")
    modo5(npoints,nfutures)
    println("MODO 6 *******************************************")
    modo6(npoints,nfutures)

  }

}
