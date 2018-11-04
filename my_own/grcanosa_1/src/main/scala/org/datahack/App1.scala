package org.datahack

//object App {
//  def main(args:Array[String]):Unit = {
//    println("Hello World!")
//  }
//}

//Otra manera
object App1 extends App {  //extends es para heredar de interfaces
  println("Hello World")
  println(List(1,2,3,4,5))
  println(List(1,2,3,4,5).map(i=>i+1))
  List(1,2,3,4,5).map(i=>i+1).foreach(i=> println(i)) //El foreach es el final de procesado, no va a devolver nada
  List(1,2,3,4,5).map(i=>i+1).foreach(println) //Se puede pasar directamente la función.
  println(List(1,2,3,4,5).map(_.toString()+"lala"))
  val printH: Any=>Unit = i => print(i.toString()+",")
  //Filter
  List(1,2,3,4,5).filter(_%2==0).foreach(printH)
  println("")
  List("a","b","c","d").filter(_ startsWith("b")).foreach(printH)
  println("")
  //FOLD LEFT
  val res = List(1,2,3,4).foldLeft(0)((acc, i) => {
    println(s"tengo $i y acc $acc")
    acc + i
  })
  println(res)
  //FOLD RIGHT
  val res2 = List(1,2,3,4).foldRight(0)((i, acc) => {
    println(s"tengo $i y acc $acc")
    acc + i
  })
  println(res2)

  //TERCER FOLD: Operaciones asociativas y conmutativas, te da igual el order
  val res3 = List(1,2,3,4).fold(0)((e1, e2) => {
    println(s"tengo $e1 y $e2")
    e1+e2
  })
  println(res3)
  //Por ejemplo, sumar el número de elementos
  val res4 = List(1,2,3,4).foldLeft(0)((acc,i)=> {
    println(s"tengo $i y acc $acc")
    if (i%2==0) {
      acc + 1
    }
    else {
      acc
    }
  })
  println(res4)

  //REDUCE. Hace lo mismo, pero sin mezclar con un elemento.
  val res5 = List(1,2,3,4).reduce(_ + _)
  println(res5)
  //val res6 = List.empty[Int].reduce(_ + _) //Esto da un error porque no hay elementos.
  //Hay otra alternativa:
  val res6 = List.empty[Int].reduceOption(_ + _)
  println("res:",res6)
  val res7 = List(1,2,3,4).reduceOption(_ + _) //El option devuelve None o Some en funcion de si hay o no valor.
  println("res:",res7)

  //FLATMAP

  println(1 to 10 toList)
  val rangeUntil: Int=>List[Int] = 1 to _ toList

  println(rangeUntil(4))
  println(rangeUntil(0))

  println(List(1,2,3,4).map(rangeUntil))
  //Si quiero tener una lista con todos estos elementos
  //Podrías hacer:
  println(List(1,2,3,4).map(rangeUntil).flatten) //Alt+Enter te da las opciones!!
  //O directamente
  println(List(1, 2, 3, 4).flatMap(rangeUntil))
  println(List(1, 2, 3, 4).flatMap(i => rangeUntil(i)))

  //GROUPBY: Devuelve un mapa, nosotros en groupBY devolvemos True o False, que serían nuestras claves
  println(List(1,2,3,4).groupBy(_%2==0))

  val names = List("carlos","fabian","gonzalo","alvaro","naomi","miriam","alberto","david","alfonso")
  println(names.groupBy(_.head))

  //Strings son listas de caracteres
  println("Alfonso".take(3))
  println("Alfonso".reverse)
  println("Alfonso".drop(3))

  println(names.takeWhile(_.length >= 6))

  val names_ord = names.sortBy(_.length)
  println(names_ord)

  println(names.span(_.length == 5))
  println(names_ord.span(_.length == 5))


  println(names.zipWithIndex)
  println(names.zipWithIndex.sortBy(_._1))
  println(names.zip(List("a","b","c","d","e")))

  println(names.zip(names.tail)) //Asi puedes tener el primer elemento con el siguiente
  println(names.zipAll(names.tail,"fulanito","menganito")) //En este caso se acaba antes la segunda lista.
                                                           // Si se acabase la primera cogeria fulanito.

  println(names.mkString(";"))
  println(names.mkString("'","'|'","'"))

  //Métodos que convierten a otras estructuras de datos

  println(names.toSet)
  println(names.toVector)

  val names_map = names.groupBy(_.head)
  println(names_map)

  println(names_map.map(t => t._1))
  println(names_map.mapValues(v => v.head))
  println(names_map.mapValues(v => v.size))


  //For Compresion
  val res222 = for {
    n <- List(1,2,3,4)      //Si el primer elemento es una lista, devuelve una lista.
                            //Si el primer elemento fuese una Seq, devuelve un Seq.
                            //Normalmente no se mezclan.
    n2 <- (1 to n).toList
  } yield s"$n2"

  println(res222)

  //Esto es igual a:
  List(1,2,3,4).flatMap(n=> (1 to n).toList.map(n2 => s"$n2"))

  //Se pueden hacer filtrados
  var result = for {
    n <-List(1,2,3,4)
    if n%2 == 0
    ndo = n* 2
    n2 <- (n to ndo).toList
  } yield s"$n2"

  println(result)


  //Formas de pasar elementos a tuplas
  val lambda1: (Int,Int) => Boolean = (i1,i2) => true
  println(lambda1(1,2))
  val tttt = (1,2)
  println(lambda1.tupled(tttt))

}


