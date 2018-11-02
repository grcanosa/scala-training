package org.grcanosa

import SpierskyTriangle._

object SpierskyTriangle{
  type Casilla = Boolean
  type Fila = List[Casilla]
  type Tri = List[Fila]
  type TriFin = List[List[Char]]

  val convertToFinal : Tri => TriFin = _.map(_.map(b => if (b) '1' else '_'))

  val getBase: Tri => Int = t => t.map(_.size).max

  def convert2Square(t:Tri): TriFin = {
    val base = getBase(t)
    val squarBool = for {
      f <- t
    } yield List.fill(base/2-f.size/2)(false) ++ f ++ List.fill(base/2-f.size/2)(false)
    convertToFinal(squarBool)
  }

  val getSubTriangle:Tri => Tri = t => t.zipWithIndex.filter(_._2 <= getBase(t)/4).map(_._1)

  val startTriangle: Int => Tri = n => for {
    r <- (0 to n/2).toList
  } yield List.fill(1+r*2)(true)

  def iterate(tri:Tri,itnum:Int): Tri ={
    itnum match {
      case n if n < 0 => println("Ops...");tri
      case 0 => tri
      case _ => {
        val subtri = getSubTriangle(tri)
        val invtri = subtri.reverse.map(_.map(_ => false))
        val it_subtri = iterate(subtri,itnum-1)
        it_subtri ++ (it_subtri,invtri).zipped.map{case (subf,invf) => subf ++ invf ++ subf}
      }
    }
  }


}





//
//case class SpierskyTriangle(triangle:Tri) {
//
//  lazy val startTri:TriEq = TriEq(base)
//
//  def iterate:SpierskyTriangle = {
//
//  }
//
//  lazy val triangle: Tri = ???
//}
