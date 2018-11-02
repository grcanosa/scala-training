package org.datahack

import org.scalatest.{Assertion, FlatSpecLike, Matchers}

class SudokuTest extends FlatSpecLike with Matchers {
  import Sudoku._
  import cats.syntax.show._
  import Sudoku.implicits._

  "A Sudoku" should "put an elements in a field when asked" in {
    EmptySudoku.putNumber((0, 1), 3) should not be EmptySudoku
  }

  it should "put the element only in the selected field" in {
    val posiiton = (3, 5)
    val sudoku = EmptySudoku.putNumber(posiiton, 2)
    sudoku.getField(posiiton) shouldBe Some(2)
    for {
      x <- RangePosition
      y <- RangePosition
      p = (x, y)
      if p != posiiton
    } yield sudoku.getField(p) shouldBe EmptyField
  }

  it should "not allow to put an element in a non valid position" in {
    def singlePosition(position: Position): List[Assertion] = {
      val sudoku = EmptySudoku.putNumber(position, 2)
      val invalidPositionsByColumn: List[Position] =
        (0 to 8).map(e => position.copy(_1 = e)).toList
      val invalidPositionsByRow: List[Position] =
        (0 to 8).map(e => position.copy(_2 = e)).toList
      val xIni = position._1 / 3 * 3
      val yIni = position._2 / 3 * 3
      val invalidPositionsBySquare: List[Position] = (for {
        x <- xIni to (xIni + 2)
        y <- yIni to (yIni + 2)
      } yield (x, y)).toList
      val invalidPositions
        : List[Position] = invalidPositionsByColumn ::: invalidPositionsByRow ::: invalidPositionsBySquare

      for {
        invalidPosition <- invalidPositions
      } yield sudoku.putNumber(invalidPosition, 2) shouldBe sudoku
    }

    for {
      p <- ValidPossitions
    } yield singlePosition(p)
  }

  it should "show corectly" in {
    val emptySudokuRepresentation = List(
      " | | || | | || | | ",
      "-+-+-++-+-+-++-+-+-",
      " | | || | | || | | ",
      "-+-+-++-+-+-++-+-+-",
      " | | || | | || | | ",
      "=+=+=++=+=+=++=+=+=",
      " | | || | | || | | ",
      "-+-+-++-+-+-++-+-+-",
      " | | || | | || | | ",
      "-+-+-++-+-+-++-+-+-",
      " | | || | | || | | ",
      "=+=+=++=+=+=++=+=+=",
      " | | || | | || | | ",
      "-+-+-++-+-+-++-+-+-",
      " | | || | | || | | ",
      "-+-+-++-+-+-++-+-+-",
      " | | || | | || | | ").mkString("\n")
    EmptySudoku.show shouldBe emptySudokuRepresentation
  }

  it should "give you the posible elements to put in a field" in {
    val position = (0, 4)
    val sudokuWithAElement = EmptySudoku.putNumber(position,1)
    sudokuWithAElement.getPosibleElementsInPosition(position) shouldBe Set.empty
    sudokuWithAElement.getPosibleElementsInPosition((1,4)) shouldBe (2 to 9).toSet
    sudokuWithAElement.getPosibleElementsInPosition((0,7)) shouldBe (2 to 9).toSet
    sudokuWithAElement.getPosibleElementsInPosition((0,3)) shouldBe (2 to 9).toSet
  }

  it should "indicate that is solved if all the cells are filled" in {
    val solvedSudokuElements = List(
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
    val solvedSudoku = initialize(ValidPossitions.zip(solvedSudokuElements):_*)
    solvedSudoku.isFinished shouldBe true

    val notYetSolved = initialize(ValidPossitions.zip(solvedSudokuElements.dropRight(1)):_*)
    notYetSolved.isFinished shouldBe false
  }
}
