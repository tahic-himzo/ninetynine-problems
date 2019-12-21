package problems.list

import org.scalatest._

import scala.util.{Failure, Try}

class P21_InsertAtSpec extends WordSpec with Matchers {
  "P21_InsertAt" should {
    "insert an item at a given index" in {
      P21_InsertAt.execute(7, 3, List(1, 1, 2, 3, 5, 8, 10, 15)) shouldEqual List(1, 1, 2, 7, 3, 5, 8, 10, 15)
      P21_InsertAt.execute(7, 8, List(1, 1, 2, 3, 5, 8, 10, 15)) shouldEqual List(1, 1, 2, 3, 5, 8, 10, 15, 7)
      P21_InsertAt.execute(7, 2, List(1, 1, 2, 3, 5, 8)) shouldEqual List(1, 1, 7, 2, 3, 5, 8)
      P21_InsertAt.execute(7, 1, List(1, 2)) shouldEqual List(1, 7, 2)
      P21_InsertAt.execute(7, 0, List(1, 2)) shouldEqual List(7, 1, 2)
    }

    "should fail on negative index" in {
      Try(P21_InsertAt.execute(7, -3, List(1, 1, 2, 3, 5, 8, 10, 15))) shouldBe a[Failure[_]]
    }

    "should fail if index is bigger than length" in {
      Try(P21_InsertAt.execute(7, 10, List(1, 1, 2, 3, 5, 8, 10, 15))) shouldBe a[Failure[_]]
      Try(P21_InsertAt.execute(7, 1, List())) shouldBe a[Failure[_]]
    }
  }
}
