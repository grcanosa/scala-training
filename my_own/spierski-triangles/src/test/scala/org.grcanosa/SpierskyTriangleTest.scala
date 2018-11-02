package org.grcanosa

import org.scalatest._
import SpierskyTriangle._

class SpierskyTriangleTest extends FunSpec with Matchers {

  describe("A Simple triangle") {
    val tri63 = startTriangle(63)

    it("should be completely equal"){
      convert2Square(tri63) shouldBe TestTriangles.testtriang0
    }

    it("should have a sub Triangle"){
      getSubTriangle(tri63) shouldBe startTriangle(31)
    }
  }



  describe("Iterations") {
    val tri63 = startTriangle(63)
 
    it("should be comlpetely equal to iteration 1") {
      convert2Square(iterate(tri63,1)) shouldBe TestTriangles.testtriang1
    }

    it("should be comlpetely equal to iteration 2") {
      convert2Square(iterate(tri63,2)) shouldBe TestTriangles.testtriang2
    }

    it("should be comlpetely equal to iteration 4") {
      convert2Square(iterate(tri63,4)) shouldBe TestTriangles.testtriang5
    }



  }
}












//    it ("should be test triangle 0 in the first rows") {
//      tri63.squareTri(0) shouldBe TestTriangles.testtriang0(0)
//      tri63.squareTri(1) shouldBe TestTriangles.testtriang0(1)
//      tri63.squareTri(2) shouldBe TestTriangles.testtriang0(2)
//      tri63.squareTri(3) shouldBe TestTriangles.testtriang0(3)
//      tri63.squareTri(4) shouldBe TestTriangles.testtriang0(4)
//    }
//
//    it("should have the same length"){
//      tri63.squareTri.size shouldBe TestTriangles.testtriang0.size
//    }

//    it("should have the same shape") {
//      for {
//        (f1,f2) <- (tri63.squareTri,TestTriangles.testtriang0).zipped
//      } yield f1.size shouldBe f2.size
//    }

//    it("should have the same lists") {
//      for {
//        (f1,f2) <- (tri63.squareTri,TestTriangles.testtriang0).zipped
//      } yield f1 shouldBe f2
//    }