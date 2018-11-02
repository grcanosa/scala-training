package org.datahack
import org.datahack.Sudoku.{ValidPossitions, initialize}

import scala.concurrent.{Await, Future, Promise}
import scala.io.StdIn.readLine

object ConsoleProgram {
  import scala.concurrent.duration._
  import Sudoku.implicits._
  import cats.syntax.show._
    val initialSudoku: Sudoku = {val solvedSudokuElements = List(
        7,3,5,6,1,4,8,9,2,
        8,4,2,9,7,3,5,6,1,
        9,6,1,2,8,5,3,7,4,
        2,8,6,3,4,9,1,5,7,
        4,1,3,8,5,7,9,2,6,
        5,7,9,1,2,6,4,3,8,
        1,5,7,4,9,2,6,8,3,
        6,9,4,7,3,8,2,1,5,
        3,2,8,5,6,1,7,4,9,
      )

      initialize(ValidPossitions.zip(solvedSudokuElements.dropRight(2)).filter{case ((x,y),e) => y <= 6 && x <= 6}:_*)
    }
  val initialSudoku2: Sudoku = {
    val solvedSudokuElements = List(
      5, 3, 0, 0, 7, 0, 0, 0, 0,
      6, 0, 0, 1, 9, 5, 0, 0, 0,
      0, 9, 8, 0, 0, 0, 0, 6, 0,
      8, 0, 0, 0, 6, 0, 0, 0, 3,
      4, 0, 0, 8, 0, 3, 0, 0, 1,
      7, 0, 0, 0, 2, 0, 0, 0, 6,
      0, 6, 0, 0, 0, 0, 2, 8, 0,
      0, 0, 0, 4, 1, 9, 0, 0, 5,
      0, 0, 0, 0, 8, 0, 0, 7, 9,
    )
    initialize(ValidPossitions.zip(solvedSudokuElements).filter { case ((x, y), e) => e != 0 }: _*)
  }
  def main(args: Array[String]): Unit = {
    console(Sudoku.EmptySudoku)
  }

  def console(sudoku: Sudoku):Unit = {
    println(sudoku.show)
    if (sudoku.isFinished)
      println("you did it!!!!!!!!!")
    else {
    print(">")
    readLine().split(" ").toList match {
      case "put" :: x :: y :: e :: _ => console(sudoku.putNumber((x.toInt, y.toInt), e.toInt))
      case "hint" :: x :: y :: _ =>
        println(sudoku.getPosibleElementsInPosition((x.toInt, y.toInt)))
        console(sudoku)
      //case "solve" :: _ => Solver.solve(sudoku, s => console(s))
      case "solve" :: _ =>
        import scala.concurrent.ExecutionContext.Implicits.global
        val prom:Promise[Sudoku] = Promise()
        val f: Future[Sudoku] = prom.future
        val futureWithPrinter = f.map(s => {
          println(s.show)
        })
        Solver.solve(sudoku, prom)
        Await.result(futureWithPrinter,10.seconds)
      case _ =>
        println("dont't know what you did there")
        console(sudoku)
    }
    }
  }

}
