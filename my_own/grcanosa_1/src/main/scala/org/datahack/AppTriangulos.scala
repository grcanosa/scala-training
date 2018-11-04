package org.datahack

object AppTriangulos {
  type Fila = List[Char]
  type Matriz = List[Fila]
  def getMatriz(nr:Int, nc:Int, c:Char):Matriz = {
    for {
      i <- (0 until nr).toList
    } yield List.fill((nc-2*i-1)/2)('_') ++ List.fill(1+2*i)(c) ++ List.fill((nc-2*i-1)/2)('_')
  }
  val nr = 32
  val nc = 63

  val triang0:List[List[Char]] = {
    for {
      i <- (0 until nr).toList
    } yield List.fill(nr-i-1)('_') ++ List.fill(1+2*i)('1') ++ List.fill(nr-i-1)('_')
  }


  def drawTriangles(n: Int) {
    //Draw the N'th iteration of the fractal as described
    // in the problem statement
    val nr = 32
    val nc = 63
    val l:List[List[Char]] = for {
      i <- (0 until nr).toList
    } yield List.fill(nr-i-1)('_') ++ List.fill(1+2*i)('1') ++ List.fill(nr-i-1)('_')

    l.foreach{l => println(l.mkString)}
  }

  def main(args: Array[String]) {
    drawTriangles(0)
  }
}
