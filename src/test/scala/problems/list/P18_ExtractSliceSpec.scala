package problems.list

import org.scalatest._

import scala.util.{Failure, Try}

class P18_ExtractSliceSpec extends WordSpec with Matchers {
  "P18_ExtractSlice" should {
    "extract a slice between 2 indexes" in {
      P18_ExtractSlice.executeAlt(3, 7, List(1, 1, 2, 3, 5, 8, 10, 15)) shouldEqual List(3, 5, 8, 10)
      P18_ExtractSlice.executeAlt(2, 5, List(1, 1, 2, 3, 5, 8)) shouldEqual List(2, 3, 5)
      P18_ExtractSlice.executeAlt(1, 1, List(1, 2)) shouldEqual List()
      P18_ExtractSlice.executeAlt(0, 0, List(1, 2, 3)) shouldEqual List()
      P18_ExtractSlice.executeAlt(0, 0, List()) shouldEqual List()
    }

    "report an error if borders are invalid" in {
      Try(P18_ExtractSlice.executeAlt(-1, 2, List(1, 2, 3))) shouldBe a[Failure[_]]
      Try(P18_ExtractSlice.executeAlt(2, 2, List(1))) shouldBe a[Failure[_]]
      Try(P18_ExtractSlice.executeAlt(3, 2, List(1, 2, 3))) shouldBe a[Failure[_]]
    }

  }
}
