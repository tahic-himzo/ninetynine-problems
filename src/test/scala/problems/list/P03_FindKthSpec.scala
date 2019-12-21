package problems.list

import org.scalatest._

import scala.util.{Failure, Try}

class P03_FindKthSpec extends WordSpec with Matchers {
  "P03_FindKth" should {
    "return the last but one element of a list" in {
      P03_FindKth.execute(3, List(1, 1, 2, 3, 5, 8)) shouldEqual 3
      P03_FindKth.execute(1, List(1, 2)) shouldEqual 2
    }

    "raise an error if the list has less than k+1 items" in {
      Try(P03_FindKth.execute(1, List(1))) shouldBe a[Failure[_]]
    }
  }
}
