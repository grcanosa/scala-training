package org.datahack

import org.datahack.Sudoku.{getCuadrantesLimits, _}
import org.scalatest.{FunSpec, Matchers}

//Mirar ScalaSuite7

class SudokuTest extends FunSpec with Matchers {

  describe("An empty Sudoku") {
    it("should have only empty casillas"){
      for {
        x <- (0 until tamañoSudoku)
        y <- (0 until tamañoSudoku)
      } yield emptySudoku.getNum(x,y) shouldBe emptyCasilla
    }

    it("should not be modified when put is out of bounds"){
      emptySudoku.putNum(-1,2,1) shouldBe emptySudoku
      emptySudoku.putNum(2,-1,1) shouldBe emptySudoku
      emptySudoku.putNum(tamañoSudoku,1,1) shouldBe emptySudoku
      emptySudoku.putNum(2,tamañoSudoku,1) shouldBe emptySudoku
    }

    it("should not modify the sudoku when the value is out of bonds"){
      emptySudoku.putNum(0,0,0) shouldBe emptySudoku
      emptySudoku.putNum(0,0,tamañoSudoku+1) shouldBe emptySudoku
    }

    it("should be modified when value is inside bounds"){
      emptySudoku.putNum(0,0,1) shouldNot be (emptySudoku)
      emptySudoku.putNum(0,0,tamañoSudoku) shouldNot be (emptySudoku)
    }

    it("should only modify a casilla with a single put "){
      val modifiedFila = 3
      val modifiedCol = 5
      val modifiedVal = 3
      val modifiedSudoku:Sudoku = emptySudoku.putNum(modifiedFila,modifiedCol,modifiedVal)
      modifiedSudoku.getNum(modifiedFila,modifiedCol) shouldBe Some(modifiedVal)
      for {
        x <- (0 until tamañoSudoku)
        y <- (0 until tamañoSudoku)
        if x != modifiedFila
        if y != modifiedCol
      } yield modifiedSudoku.matriz(x)(y) shouldBe emptyCasilla
    }

    it("should not modify an already set cell"){
      val modifiedFila = 3
      val modifiedCol = 5
      val modifiedVal = 3
      val modifiedSudoku = emptySudoku.putNum(modifiedFila,modifiedCol,modifiedCol)
      val modifiedSudoku2 = modifiedSudoku.putNum(modifiedFila,modifiedCol,modifiedCol)
      modifiedSudoku2 shouldBe modifiedSudoku
    }

    it("should return all numbers as possibilities for any cell"){
      for {
        f <- 0 until tamañoSudoku
        c <- 0 until tamañoSudoku
      } yield emptySudoku.getOpciones(f,c) shouldBe List(1,2,3,4,5,6,7,8,9)
    }
  }


  describe("A test Sudoku") {

    val testFila = (0 until tamañoSudoku).toList.map(n => Some(n))
    val testSudoku = new Sudoku(List.fill(tamañoSudoku)(testFila))

    it("should be solved (altough incorrectly)"){
      testSudoku.isSolved shouldBe true
    }

    it("should get the row number in a matrix filled with the col number"){
      for {
        f <- (0 until tamañoSudoku)
        c <- (0 until tamañoSudoku)
      } yield testSudoku.getNum(f,c) shouldBe testFila(c)
    }

    it("should return None if out of bounds"){
      testSudoku.getNum(-1,1) shouldBe None
      testSudoku.getNum(2,-1) shouldBe None
      testSudoku.getNum(tamañoSudoku,1) shouldBe None
      testSudoku.getNum(2,tamañoSudoku) shouldBe None
    }

    it("should not be modified when put is out of bonds"){
      testSudoku.putNum(-1,2,1) shouldBe testSudoku
      testSudoku.putNum(2,-1,1) shouldBe testSudoku
      testSudoku.putNum(tamañoSudoku,1,1) shouldBe testSudoku
      testSudoku.putNum(2,tamañoSudoku,1) shouldBe testSudoku
    }

    it("should not be modified when the value is out of bonds"){
      testSudoku.putNum(0,0,0) shouldBe testSudoku
      testSudoku.putNum(0,0,tamañoSudoku+1) shouldBe testSudoku
    }



    it("should not be modified if you try to modify an already modified value"){
      for {
        f <- 0 until tamañoSudoku
        c <- 0 until tamañoSudoku
      } yield testSudoku.putNum(f,c,0) shouldBe testSudoku
    }

    it("should return a column when that column is requested"){
      for {
        c <- 0 until tamañoSudoku
      } yield testSudoku.getColumna(c) shouldBe List.fill(tamañoSudoku)(Some(c))
    }

    it("should return exptected quadrant when requested"){
      testSudoku.getCuadrante(0,2,0,2) shouldBe List(0,1,2,0,1,2,0,1,2).map(Some(_))
      testSudoku.getCuadrante(0,2,3,5) shouldBe List(3,4,5,3,4,5,3,4,5).map(Some(_))
      testSudoku.getCuadrante(0,2,6,8) shouldBe List(6,7,8,6,7,8,6,7,8).map(Some(_))
      testSudoku.getCuadrante(3,5,0,2) shouldBe List(0,1,2,0,1,2,0,1,2).map(Some(_))
      testSudoku.getCuadrante(3,5,3,5) shouldBe List(3,4,5,3,4,5,3,4,5).map(Some(_))
      testSudoku.getCuadrante(3,5,6,8) shouldBe List(6,7,8,6,7,8,6,7,8).map(Some(_))
      testSudoku.getCuadrante(6,8,0,2) shouldBe List(0,1,2,0,1,2,0,1,2).map(Some(_))
      testSudoku.getCuadrante(6,8,3,5) shouldBe List(3,4,5,3,4,5,3,4,5).map(Some(_))
      testSudoku.getCuadrante(6,8,6,8) shouldBe List(6,7,8,6,7,8,6,7,8).map(Some(_))
    }

    it("should return correct quadrant limits") {
      getCuadrantesLimits(0) shouldBe (0,2)
      getCuadrantesLimits(1) shouldBe (0,2)
      getCuadrantesLimits(2) shouldBe (0,2)
      getCuadrantesLimits(3) shouldBe (3,5)
      getCuadrantesLimits(4) shouldBe (3,5)
      getCuadrantesLimits(5) shouldBe (3,5)
      getCuadrantesLimits(6) shouldBe (6,8)
      getCuadrantesLimits(7) shouldBe (6,8)
      getCuadrantesLimits(8) shouldBe (6,8)
    }

    it("should return expected quadrant when row and colum are given") {
      testSudoku.getCuadrante(0,0) shouldBe testSudoku.getCuadrante(0,2,0,2)
      testSudoku.getCuadrante(0,1) shouldBe testSudoku.getCuadrante(0,2,0,2)
      testSudoku.getCuadrante(0,2) shouldBe testSudoku.getCuadrante(0,2,0,2)
      testSudoku.getCuadrante(2,0) shouldBe testSudoku.getCuadrante(0,2,0,2)
      testSudoku.getCuadrante(2,1) shouldBe testSudoku.getCuadrante(0,2,0,2)
      testSudoku.getCuadrante(2,2) shouldBe testSudoku.getCuadrante(0,2,0,2)
    }
  }

  describe("Getting possibilities") {
    it("should return a single number in a row with all but one elements defined"){
      val testSudoku = buildSoduku((0 until tamañoSudoku-1).toList.map(n => (0,n,n+1)))
      testSudoku.getOpciones2(0,8) shouldBe List(9)
    }
    it("should return a single number in a column with all but one elements defined"){
      val testSudoku = buildSoduku((0 until tamañoSudoku-1).toList.map(n => (n,0,n+1)))
      testSudoku.getOpciones2(8,0) shouldBe List(9)
    }
    it("should return a single number in a quadrant with all but one elements defined"){
      val testSudoku = buildSoduku(List((0,0,1),(0,1,2),(0,2,3),(1,0,4),(1,1,5),(1,2,6),(2,0,7),(2,1,8)))
      testSudoku.getOpciones2(2,2) shouldBe List(9)
    }
  }

  describe("Extract Elements method") {
    val testList = (0 until 9).toList
    it("should return same list if limits are given") {

      extractElements(testList, 0, 8) shouldBe testList
    }
    it("should return some elements if different limits are given") {
      extractElements(testList, 1, 8) shouldBe testList.drop(1)
    }

    it("should return first element if 0 is provided as both limits") {
      extractElements(testList, 0, 0) shouldBe List(testList(0))
    }

    it("should return last element if size is provided as both limits") {
      val nhigh = testList.size - 1
      extractElements(testList, nhigh, nhigh) shouldBe List(testList(nhigh))
    }
  }
  describe("Sudoku validation") {

    val testFila = (0 until tamañoSudoku).toList.map(n => Some(n))
    val testSudoku = new Sudoku(List.fill(tamañoSudoku)(testFila))

    it("should validate an empty row"){
      emptyFila.check shouldBe true
    }

    it("should validate a single value row")  {
      emptySudoku.putNum(0,0,1).getFila(0).check shouldBe true
    }

    it("should validate a row with different values") {
      //println("None empty fila")
      testSudoku.getFila(0).check shouldBe true
    }




  }



}
