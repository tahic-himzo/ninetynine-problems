package problems.list

import org.scalatest._

import scala.util.{Failure, Try}

class P16_DropEveryNSpec extends WordSpec with Matchers {
  "P16_DropEveryN" should {
    "drop every n-th item in a list" in {
      P16_DropEveryN.execute(3, List(1, 1, 2, 3, 5, 8)) shouldEqual List(1, 1, 3, 5)
      P16_DropEveryN.execute(2, List(1, 2)) shouldEqual List(1)
      P16_DropEveryN.execute(2, List(1)) shouldEqual List(1)
      P16_DropEveryN.execute(3, List()) shouldEqual List()
    }

    "report an error if n <= 0" in {
      Try(P16_DropEveryN.execute(0, List(1,2,3))) shouldBe a[Failure[_]]
    }

  }
}
