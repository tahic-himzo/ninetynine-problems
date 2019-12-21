package problems.list

import org.scalatest._

import scala.util.{Failure, Try}

class P02_PenultimateSpec extends WordSpec with Matchers {
  "P02_Penultimate" should {
    "return the last but one element of a list" in {
      P02_Penultimate.execute(List(1, 1, 2, 3, 5, 8)) shouldEqual 5
      P02_Penultimate.execute(List(1, 2)) shouldEqual 1
    }

    "raise an error if the list has less than two items" in {
      Try(P02_Penultimate.execute(List(1))) shouldBe a[Failure[_]]
    }
  }
}
