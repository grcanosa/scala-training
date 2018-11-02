package org.datahack

import cats.Show
import org.datahack.Sudoku._

object Sudoku {
  type Field = Option[Int]
  type Line = List[Field]
  type Matrix = List[Line]
  type Position = (Int,Int)

  private val MinPosition = 0
  private val MaxPosition = 8
  val RangePosition: Range.Inclusive = MinPosition to MaxPosition
  val ValidPossitions: List[(Int, Int)] = for {
    y <- RangePosition.toList
    x <- RangePosition
  } yield (x,y)
  val MinValue = 1
  val MaxValue = 9
  val ValueRange: Range.Inclusive = MinValue to MaxValue
  val EmptyField: Field = None
  private val EmptyLine: Line = (1 to 9).map(_ => EmptyField).toList
  private val EmptyMatrix: Matrix = (1 to 9).map(_ => EmptyLine).toList
  val EmptySudoku: Sudoku = new Sudoku(EmptyMatrix)

  def initialize(c:(Position,Int)*):Sudoku =
    c.foldLeft(EmptySudoku){case (s,(p,e)) => s.putNumber(p,e)}

  object implicits {
    import cats.syntax.show._

    private val simpleLine = "\n-+-+-++-+-+-++-+-+-\n"
    private val complexLine = "\n=+=+=++=+=+=++=+=+=\n"
    private val simpleColumn = "|"
    private val complexColumn = "||"

    implicit val someIntShow:Show[Field] = (t: Option[Int]) => t.fold(" ")(_.toString)
    implicit val someRow:Show[Line] = (t: Line) => {
      t.map(_.show).grouped(3).map(_.mkString(simpleColumn)).mkString(complexColumn)
    }
    implicit val showSudoku:Show[Sudoku] = (t: Sudoku) => t.matrix.map(_.show).grouped(3).map(_.mkString(simpleLine)).mkString(complexLine)
  }

  private object implicitsUtils {
    implicit class ListUtils[T](l: List[T]) {
      def modifyPosition(i: Int, modification: T => T): List[T] = {
        l.zipWithIndex.map {
          case (elemToModify, `i`) => modification(elemToModify)
          case (validElement, _)   => validElement
        }
      }
      def putInPosition(i: Int, element: T): List[T] = {
        l.modifyPosition(i, _ => element)
      }
      def getThird(i:Int):List[T] = {
        val start = i/3 * 3
        l.slice(start, start + 3)
      }
    }
  }
}

case class Sudoku(matrix: Matrix) {

  import Sudoku.implicitsUtils._

  def getField(position:Position): Field =
    matrix.lift(position._2).flatMap(_.lift(position._1)).flatten

  lazy val freeSpaces:Int = MaxValue*MaxValue - matrix.flatten.flatten.size
  lazy val isFinished: Boolean = freeSpaces == 0

  private def getNonValidByRow(y:Int):Set[Int] = matrix(y).flatten.toSet
  private def getNonValidByColumn(x:Int):Set[Int] = matrix.flatMap(_(x)).toSet
  private def getNonValidBySquare(p:Position):Set[Int] = matrix.getThird(p._2).flatMap(_.getThird(p._1)).flatten.toSet
  private def getInvalidElementsInPosition(p:Position):Set[Int] = getNonValidBySquare(p) ++ getNonValidByColumn(p._1) ++ getNonValidByRow(p._2)

  def getPosibleElementsInPosition(p:Position):Set[Int] =
    if (getField(p) == EmptyField)
      ValueRange.toSet.diff(getInvalidElementsInPosition(p))
    else
      Set.empty

  private def isInvalidElementInPosition(p:Position,e:Int):Boolean = getInvalidElementsInPosition(p).contains(e)
  private def isValidElementInPosition(p:Position,e:Int):Boolean = !isInvalidElementInPosition(p,e)

  def validNumber(e: Int): Boolean = {
    e >= MinValue && e <= MaxValue
  }

  def putNumber(position:Position, e: Int): Sudoku = {
    if (!validNumber(e) || getField(position) != EmptyField || isInvalidElementInPosition(position,e)) {
      this
    } else {
      new Sudoku(matrix.modifyPosition(position._2, _.putInPosition(position._1, Some(e))))
    }
  }
}
