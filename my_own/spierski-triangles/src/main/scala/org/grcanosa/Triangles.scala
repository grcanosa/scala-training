package org.grcanosa

import Triangles._

object Triangles {
  val nRows = 32
  val nCols = 63

  type Casilla = Boolean
  type Fila = List[Casilla]
  type Matriz = List[Fila]
  type MatrizFinal = List[List[Char]]

  val convertToFinal : Matriz => MatrizFinal = _.map(_.map(b => if (b) '1' else '_'))

  val triang0:Matriz = for{
    i <- (0 until nRows).toList
  } yield List.fill(nRows-i-1)(false) ++ List.fill(1+2*i)(true) ++ List.fill(nRows-i-1)(false)

  val triang1:Matriz = triang0.zipWithIndex.map{
    case (row,i) if i < nRows/2 => row
    case (row,i) => ???
  }




}

case class Triangles(matriz:Matriz) {


}
