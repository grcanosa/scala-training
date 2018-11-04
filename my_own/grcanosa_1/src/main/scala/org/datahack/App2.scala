package org.datahack

object App2 {
  def main(args:Array[String]):Unit = {
    //ERROR HANDLING
    println("ERROR HANDLING ************************** ")

    //OPTION
    println("OPTIONS ********************************* ")

    val hasValue: Option[Int] = Option(4)
    //val doesnotHave: Option[Int] = Option(Null)

    val printOp: Option[Int] => Unit = _ => ()


    println("IMPLICITOS ****************************** ")

    val op = {
      implicit val theNumber = 42

      def sumNumbers(a: Int)(implicit b: Int): Int = {
        a + b
      }

      println(sumNumbers(34)(15))
      println(sumNumbers(12))
    }
//    val op3 = {
//      def sumNumbers(a: Int)(implicit b: Int): Int = {
//        a + b
//      }
//
//      println(sumNumbers(34)(15))
//      println(sumNumbers(12))
//
//      //AQUI DARIA ERROR!!!
//    }

    //No solo los valores son implicitos!!!

    //Conversiones implicitas

    //Clases implicitas
    //Esto solo acepta un elemento, que es de un tipo en concreto.
    implicit class StringExtension(s:String){
      def dameDos:String = s+s
      def dameFabi:String = "fabian"
    }

    "my string".dameDos
    "gonzalo".dameFabi

    implicit class StringEx2(s:String){
      def unary_! = s.toUpperCase
      //def unary_! : String = "pepe"+s+"pepe"
      def unary_- : String = "#"+s+"#"

    }
    println(!"gonzalo")
    println(-"gonzalo")

    }

}



