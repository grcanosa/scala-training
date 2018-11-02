package org.datahack

import scala.util.Try


/**
 * @author ${user.name}
 */
object App {
  import Sudoku._


  val LS1 = List((0,1,3),(0,2,5),(0,6,8),(0,7,4),(0,8,2)
                ,(1,3,4),(1,4,8),(1,5,2),(1,7,3),(1,8,9)
                ,(2,1,8),(2,3,5)
                ,(3,1,9),(3,5,4),(3,7,6),(3,8,7)
                ,(4,3,9),(4,4,6),(4,5,1)
                ,(5,0,4),(5,1,5),(5,3,8),(5,7,9)
                ,(6,5,9),(6,7,8)
                ,(7,0,6),(7,1,4),(7,3,7),(7,4,1),(7,5,8)
                ,(8,0,8),(8,1,1),(8,2,2),(8,6,9),(8,7,7))


  val LS2 = "*567*8**1;2*1******;97*6*3*2*;*14*3*962;*********;387*6*51*;*2*1*6*43;******1*6;1**4*527*"

  def convertToInt(s:String):Try[Int] = {
      Try{s.toInt}
  }

  def insertaElemento(sudoku: Sudoku):Sudoku= {
    println("Fila?:")
    val filaStr = scala.io.StdIn.readLine()
    val f = convertToInt(filaStr)
    if(f.isFailure){
      println("Introduce un numero!")
      sudoku
    }
    else
    {
      println("Columna?:")
      val colStr = scala.io.StdIn.readLine()
      val c = convertToInt(colStr)
      if(c.isFailure){
        println("Introduce un numero!")
        sudoku
      }
      else {
        println("Número?:")
        val numStr = scala.io.StdIn.readLine()
        val n = convertToInt(numStr)
        if(n.isFailure) {
          println("Introduce un numero!")
          sudoku
        }
        else{
          println("Has elegido fila: "+filaStr+", y columna: "+colStr+", y el numero: "+numStr)
          sudoku.putNum(f.get,c.get,n.get)
        }
      }
    }
  }

  def consola(s:Sudoku=emptySudoku):Unit = {
    println("Elige opcion: ")
    println("1 => Inserta elemento")
    println("2 => Muestra sudoku")
    println("3 => Salir")
    val inStr = scala.io.StdIn.readLine()
    inStr match {
      case "1" => val s2 = insertaElemento(s);consola(s2)
      case "2" => s.show2;consola(s)
      case "3" => println("Adios")
      case _ => println("Invalid option");consola(s)
    }
  }



  def main(args : Array[String]) {
    val SS2 = buildSudokuFromString(LS2)
//    s2.show2
//    emptySudoku.show
//
//    emptySudoku.putNum(0,0,1).show
//
//
//    emptySudoku.putNum(0,0,1).putNum(0,0,2).show
//
//
//    buildSoduku(List((2,5,9))).show
//
//    buildSoduku(List((0,0,1),(7,2,8),(1,3,4),(5,6,7),(8,8,9))).show2

//    val s1 = buildSoduku(LS1)
//    s1.getListaOpciones.foreach(println(_))
//    val s2 = solveSudoku(s1)
//    s2.show2
//    println("")
//    s2.getListaOpciones.foreach(println(_))

    //val s3 = solveSudoku(emptySudoku)

  //consola(emptySudoku)
    SS2.show2
    val solution = solveSudoku2(SS2)
//    println(solution == SS2)
    solution.show2
//    val emptySudokuSol = solveSudoku2(emptySudoku)
//    emptySudokuSol.show2
  }

}
