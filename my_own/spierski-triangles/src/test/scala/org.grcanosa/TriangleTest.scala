package org.grcanosa

import org.scalatest._
import Triangles._

class TrianglesTest extends FunSpec with Matchers {

  describe("Start triangle") {
    it("should be correct for iteration 0") {
      convertToFinal(triang0) shouldBe TestTriangles.testtriang0
    }
  }


}

