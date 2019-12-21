package problems.list

import org.scalatest._

import scala.util.{Failure, Try}

class P01_LastElementSpec extends WordSpec with Matchers {
  "P01_LastElement" should {
    "return the last element of a list" in {
      P01_LastElement.execute(List(1, 1, 2, 3, 5, 8)) shouldEqual 8
      P01_LastElement.execute(List(1)) shouldEqual 1
    }

    "raise an error if the list is empty" in {
      Try(P01_LastElement.execute(List.empty[Int])) shouldBe a[Failure[_]]
    }
  }
}
