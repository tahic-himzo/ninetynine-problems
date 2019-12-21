package problems.list

import org.scalatest._

import scala.util.{Failure, Try}

class P20_RemoveKthSpec extends WordSpec with Matchers {
  "P20_RemoveKth" should {
    "remove the element at a given index" in {
      P20_RemoveKth.execute(3, List(1, 1, 2, 3, 5, 8, 10, 15)) shouldEqual (List(1, 1, 2, 5, 8, 10, 15), 3)
      P20_RemoveKth.execute(2, List(1, 1, 2, 3, 5, 8)) shouldEqual (List(1, 1, 3, 5, 8), 2)
      P20_RemoveKth.execute(1, List(1, 2)) shouldEqual (List(1), 2)
      P20_RemoveKth.execute(0, List(1, 2)) shouldEqual (List(2), 1)
    }

    "should fail on negative index" in {
      Try(P20_RemoveKth.execute(-3, List(1, 1, 2, 3, 5, 8, 10, 15))) shouldBe a[Failure[_]]
    }

    "should fail if index is bigger than length" in {
      Try(P20_RemoveKth.execute(10, List(1, 1, 2, 3, 5, 8, 10, 15))) shouldBe a[Failure[_]]
      Try(P20_RemoveKth.execute(0, List())) shouldBe a[Failure[_]]
    }
  }
}
