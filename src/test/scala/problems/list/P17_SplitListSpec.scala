package problems.list

import org.scalatest._

import scala.util.{Failure, Try}

class P17_SplitListSpec extends WordSpec with Matchers {
  "P17_SplitList" should {
    "split a list at a given index" in {
      P17_SplitList.executeAlt(3, List(1, 1, 2, 3, 5, 8)) shouldEqual (List(1, 1, 2), List(3, 5, 8))
      P17_SplitList.executeAlt(2, List(1, 2)) shouldEqual (List(1, 2), List())
      P17_SplitList.executeAlt(2, List(1)) shouldEqual (List(1), List())
      P17_SplitList.executeAlt(3, List()) shouldEqual (List(), List())
      P17_SplitList.executeAlt(0, List(1, 2, 3)) shouldEqual (List(), List(1, 2, 3))
    }

    "report an error if n < 0" in {
      Try(P17_SplitList.executeAlt(-1, List(1, 2, 3))) shouldBe a[Failure[_]]
    }

  }
}
