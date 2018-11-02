package org.datahack

import scala.concurrent.Promise

object Solver {
  import Sudoku._

  private val getAllPosibleElements: Sudoku => List[(Position, Set[Int])] = (sudoku: Sudoku) => for {
    p <- ValidPossitions
    possibleValues = sudoku.getPosibleElementsInPosition(p)
    if possibleValues.nonEmpty
  } yield p -> possibleValues

  private val getEasyPositions:List[(Position, Set[Int])] => List[(Position, Int)] = posiblePositions => posiblePositions.withFilter(_._2.size == 1).map(t => (t._1, t._2.head))

  def solve(sudoku: Sudoku, ifSolved: Promise[Sudoku]): Unit = {
    if (ifSolved.isCompleted) {
      ()
    } else {
      if (sudoku.isFinished) {
        if (!ifSolved.isCompleted) {
          ifSolved.success(sudoku)
        }
      } else {
        val positionsAndValues = getAllPosibleElements(sudoku)
        if (positionsAndValues.nonEmpty) {
          val sureElements = getEasyPositions(positionsAndValues)
          if (sureElements.nonEmpty) {
            val sudokuWithSureElements = sureElements.foldLeft(sudoku) {
              case (s, (p, element)) =>
                s.putNumber(p, element)
            }
            solve(sudokuWithSureElements, ifSolved)
          } else {
            val (p, possibleElements) = positionsAndValues.minBy(_._2.size)
            possibleElements.foreach(e => solve(sudoku.putNumber(p, e), ifSolved))
          }
        } else {
          ()
        }
      }
    }
  }
}
