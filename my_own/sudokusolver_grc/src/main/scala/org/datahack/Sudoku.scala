package org.datahack

import java.security.KeyStore.TrustedCertificateEntry

import org.datahack.Sudoku.{Casilla, Matriz}


object Sudoku{
  //Mirar VisualVM para mirar el estado de los procesos de Java.

  type Casilla = Option[Int]
  type Fila = List[Casilla]
  type Matriz = List[Fila]
  type Opciones = List[Int]
  type ListaOpciones = List[(Int,Int,Opciones)]

  val tamañoSudoku:Int = 9
  val cuadrantesSudoku:Int = 3
  val tamañoCuadrante:Int = tamañoSudoku / cuadrantesSudoku
  val emptyCasilla:Casilla = None
  val emptyFila:Fila = (1 to tamañoSudoku).toList.map(_ => emptyCasilla)
  val emptyMatriz:Matriz = List.fill(tamañoSudoku)(emptyFila)
  val emptySudoku:Sudoku = new Sudoku(emptyMatriz)

  def buildSoduku(l:List[(Int,Int,Int)]):Sudoku = {
    l.foldLeft(emptySudoku){case (s,(f,c,n)) => s.putNum(f,c,n)}
  }

  def buildSudokuFromString(s:String):Sudoku = {
    buildSoduku(s.split(";")
            .toList.zipWithIndex
            .flatMap{case (s,f) => s.toList.zipWithIndex.map{case (ch,c) => (f,c,ch.toString)}}
            .filter(_._3 != "*")
            .map{case (f,c,ch) => (f,c,ch.toInt)})
  }

  val checkLimits:Int => Boolean = n => n < tamañoSudoku && n >= 0

  def getCuadrantesLimits(n:Int): (Int,Int) = {
    ((n/tamañoCuadrante) * tamañoCuadrante
      , (n/tamañoCuadrante) * tamañoCuadrante + tamañoCuadrante -1)
  }

  implicit class FilaValidation(f:Fila){
    def check:Boolean = f.filter(_.isDefined).distinct.size == f.count(_.isDefined)
  }


  def extractElements[T](l:List[T],nlow:Int,nhigh:Int):List[T] = {
    l.drop(nlow).dropRight(l.size - nhigh - 1)
  }


  def getExtractor[T](nlow:Int,nhigh:Int): List[T] => List[T] = {
    l:List[T] => l.drop(nlow).dropRight(l.size - nhigh - 1)
  }




  implicit class ListExtension[T](val l:List[T]){
    def cambia(I:Int,f:T => T):List[T] = l.zipWithIndex.map {
      case (lsub, I) => f(lsub)
      case (lsub, _) => lsub
    }
    def printa(s:String,f:T => Unit):Unit = {
      l.zipWithIndex.foreach {
        case (ll,i) if i % tamañoCuadrante == 0 => print(s);f(ll)
        case (ll,_) => f(ll)
      }
    }
  }

  implicit class CasillaExtension(val c:Casilla){
    def printa:Unit = if(c.isDefined) print(c.get+" ") else print("_ ")
  }

  def solveSudoku(s:Sudoku):Sudoku = {
    println("==========================================")
    s.show2
    val opL = s.getListaOpciones
    println("")
    opL.filter(_._3.size <= 1).foreach(println(_))
    val s2 = opL.filter(_._3.size == 1)
                              .foldLeft(s){case (s,(f,c,List(x))) => s.putNum(f,c,x)}

    s2.show2
    if(s != s2)
      solveSudoku(s2)
    else
      s
  }

  type ESudoku = Either[String,Sudoku]

  def solveSudoku2(in_sudoku:Sudoku):Sudoku = {
    def sudokuBack(eSudoku: ESudoku):List[ESudoku] = {
      eSudoku match {
        case Left(s) => List(eSudoku)
        case Right(sudo) => {
          if(sudo.isSolved){
            println("SOLVED")
            sudo.show2
            List(eSudoku)
          }else{
            val opciones:ListaOpciones = sudo.getListaOpciones
            if(!opciones.exists(_._3.isEmpty)){
              //println("BUSCANDO OPCIONES")
              //Aplicamos opciones de 1
              val s2 = opciones.filter(_._3.size == 1)
                                .foldLeft(sudo){case (sudo,(f,c,List(x))) => sudo.putNum(f,c,x)}
              val min_n_op = opciones.map(_._3.size).min
              val next_cell_op = opciones.filter(_._3.size == min_n_op)(0)
              val my_list = for {
                num <- next_cell_op._3
                s3 = s2.putNum(next_cell_op._1,next_cell_op._2,num)
                //if s3 != s2
              } yield Right(s3)
              //println(my_list)
              my_list.flatMap(sudokuBack)
            }
            else{
              //println("EMPTY OPTIONS SUDOKU")
              List(Left("Empty Sudoku"))
            }
          }
        }
      }
    }
    val sols = sudokuBack(Right(in_sudoku)).filter(_.isRight)
    if(sols.isEmpty)
      in_sudoku
    else{
      sols(0) match{
        case Left(s) => in_sudoku
        case Right(s) => s
      }
    }

  }

//  def solveSudoku2(s:Sudoku):Sudoku = {
//    def auxsolver()
//  }



}


//Hacer un implicit class que coja una lista de "algo" y devuelva la modificacion
//si el indice es el que queremos y devuelva lo mismo si el indice

//estaría bien tambien tener una función que le pasamos los limites y te de esos
// elementos de una lista, pero de forma generica.


//Si empezamos con el sudoku vacio y tenemos una lista de posiciones y elementos
// podemos hacer el fold.



case class Sudoku(val matriz:Matriz) {
  import Sudoku._

  //Esto se podría poner como lazy val porque no cambia!! (ya que el sudoku es inmutable
  def isSolved:Boolean = {
    matriz.flatMap(_.filter(_.isEmpty)).isEmpty
  }

  def putNum(f:Int,c:Int,value:Int):Sudoku = {
    if(checkLimits(f) && checkLimits(c) && checkLimits(value -1 ) && getNum(f,c).isEmpty){
      new Sudoku(matriz.cambia(f,_.cambia(c,_ => Some(value))))
    } else {
      this
    }
  }

  def putNum2(FilaPos:Int,ColPos:Int,value:Int): Sudoku = {
    if (checkLimits(FilaPos)
      && checkLimits(ColPos)
      && checkLimits(value - 1)
      && this.getNum(FilaPos,ColPos).isEmpty) {
      //matriz(fila)(columna) = n
      val matrizChange = matriz.zipWithIndex.map {
        case (sudokuFila, FilaPos) => sudokuFila.zipWithIndex.map {
          case (sudokuCelda, ColPos) => Some(value)
          case (sudokuCelda, _) => sudokuCelda
        }
        case (sudokuFila, _) => sudokuFila
      }
      val s2 = new Sudoku(matrizChange)
      if (s2.checkCasilla(FilaPos, ColPos)) {
        //println("NEW")
        //s2.show
        s2
      }
      else  {
         this
        }
    }
    else {
      //println("SAME")
      this
    }

  }

  def getNum(fila:Int,columna:Int): Casilla = {

    val maybeCasilla: Option[Casilla] = matriz.lift(fila).flatMap(_.lift(columna))
    maybeCasilla.flatten

//    if(checkLimits(fila) && checkLimits(columna))
//      matriz(fila)(columna)
//    else
//      None
  }

  def getFila(fila:Int): Fila = {
    matriz(fila)
  }

  def getColumna(columna:Int): Fila = {
    matriz.flatMap(getExtractor(columna,columna))
  }

  def getCuadrante(f:Int,c:Int):Fila = {
    val fcuad = getCuadrantesLimits(f)
    val ccuad = getCuadrantesLimits(c)
    getCuadrante(fcuad._1,fcuad._2,ccuad._1,ccuad._2)
  }

  def getCuadrante(f1:Int,f2:Int,c1:Int,c2:Int): Fila = {
    getExtractor(f1,f2)(matriz).flatMap(getExtractor(c1,c2))
  }

  def checkCasilla(f:Int,c:Int):Boolean = {
    getFila(f).check && getColumna(c).check && getCuadrante(f,c).check
  }

  def show:Unit = {
    val l = for {
      f <- 0 until tamañoSudoku
      c <- 0 until tamañoSudoku
      char = if (matriz(f)(c).isEmpty) "_" else matriz(f)(c).get.toString
      char2 = (if (c==0) "\n" else "") + char
    } yield (char2)
    l.foreach(print(_))
    println("")
  }

  def show2: Unit = {
    val printR:Fila => Unit = f => {
      println("")
      f.printa("|",_.printa)
    }
    matriz.printa("\n---------------------",printR)
    println("")
  }

  def getOpciones2(f:Int,c:Int):List[Int] = {
    val fset:Set[Int] = getFila(f).filter(_.isDefined).map(_.get).toSet
    val cset:Set[Int] = getColumna(c).filter(_.isDefined).map(_.get).toSet
    val qset:Set[Int] = getCuadrante(f,c).filter(_.isDefined).map(_.get).toSet
    ((1 to tamañoSudoku).toSet -- (fset ++ cset ++ qset)).toList
  }

  def getOpciones(f:Int,c:Int):List[Int] = {
    for {
      n <- (1 to tamañoSudoku).toList
      s2 = putNum(f,c,n)
      if this != s2
      if s2.checkCasilla(f,c)
    } yield n
    //    (1 until tamañoSudoku).map(n => (n,s.putNum(f,c,n)))
    //                          .map{case (n,s) => (n, s.getFila(f).check && s.getColumna(c).check && s.getCuadrante(f,c).check)}
    //                          .filter(_._2 == true)
    //                          .map{case (n,s) => n}
  }

  def getListaOpciones:List[(Int,Int,List[Int])] = {
    val l = for {
      f <- (0 until tamañoSudoku).toList
      c <- (0 until tamañoSudoku).toList
      if matriz(f)(c).isEmpty
    } yield (f,c,getOpciones(f,c))
    l
  }

}

