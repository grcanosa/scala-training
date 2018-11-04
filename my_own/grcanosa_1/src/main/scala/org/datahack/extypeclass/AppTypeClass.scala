package org.datahack.extypeclass

object AppTypeClass {

  trait Show[A]{
    def show(element:A):String
  }

 // Con object también se puede hacer, pero el val es el valor más parecido.
  //Poniendo los dos funciona.
//  object StringShow extends Show[String]{
//    override def show(s:String):String = s
//  }
//
//  object IntShow extends Show[Int]{
//    override def show(element: Int): String = element.toString
//  }

  implicit class ShowOps[T](e:T){
    def show2(implicit show:Show[T]):String = show.show(e)
  }

  //Hay que añadir implicit, sino al poner 4.show(...) habría que ponerlo también en los objetos.

  implicit val stringShow:Show[String] = (elem:String) => elem
  implicit val intShow:Show[Int] = (el:Int) => el.toString

  implicit def listShow[T](implicit showT: Show[T]):Show[List[T]] = {
        //(lista:List[T]) => "MI LISTAAAAA:"+lista.map(_.show2).mkString(",")
          (lista:List[T]) => "MI LISTAAAAA:"+lista.map(showT.show(_)).mkString(",")
  }

//  Otro posible método es:
//  implicit def listShow3[T](implicit showT: Show[T]):Show[List[T]] = new Show[List[T]] {
//    override def show(element: List[T]): String = element.map(showT.show(_)).mkString(";")
//  }

  //Esto ya no hace falta puesto que ya está definido con todo lo anterior.
//  implicit def listListShow[T](implicit showLT:Show[List[T]]):Show[List[List[T]]] = {
//    (ll:List[List[T]]) => "LL("+ll.map(showLT.show(_)).mkString(" |#| ")+")"
//  }


      //implicit val listShow:Show[List[T]]

      def main(args: Array[String]): Unit = {
        intShow.show(element = 4)
        stringShow.show(element = "pepe")
//        IntShow.show(element = 4)
//        StringShow.show(s = "pepe")

        //Con el ShowOps
        println(4.show2 + "pepe".show2 )
        //println(listShow)
        println(listShow[Int].show(List(1,2,3,4,5)))
        println(List(1,2,3,4).show2)
        println(List(List(1,2,3),List(4,5,6)).show2)
        //println(List(List(1,2),List(4,5)))
      }

}
