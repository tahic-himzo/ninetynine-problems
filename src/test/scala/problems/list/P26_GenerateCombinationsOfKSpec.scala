package problems.list

import org.scalatest._

import scala.util.{Failure, Try}

class P26_GenerateCombinationsOfKSpec extends WordSpec with Matchers {
  "P26_GenerateCombinationsOfK" should {
    "generate all possible combinations of k items in a list" in {
      P26_GenerateCombinationsOfK.execute(3, List(1, 2, 3, 4)) shouldEqual List(
        List(1, 2, 3),
        List(1, 2, 4),
        List(1, 3, 4),
        List(2, 3, 4),
      )
      P26_GenerateCombinationsOfK.execute(2, List(1, 2, 3)) shouldEqual List(
        List(1, 2),
        List(1, 3),
        List(2, 3)
      )
      P26_GenerateCombinationsOfK.execute(2, List(1, 2)) shouldEqual List(List(1, 2))
      P26_GenerateCombinationsOfK.execute(1, List(1)) shouldEqual List(List(1))
      P26_GenerateCombinationsOfK.execute(0, List(1)) shouldEqual List(List())
      P26_GenerateCombinationsOfK.execute(0, List(0)) shouldEqual List(List())
    }

    "fail if k is bigger than the list" in {
      Try(P26_GenerateCombinationsOfK.execute(4, List(1, 2, 3))) shouldBe a[Failure[_]]
    }

    "fail if k is negative" in {
      Try(P26_GenerateCombinationsOfK.execute(-1, List(1, 2, 3))) shouldBe a[Failure[_]]
    }
  }
}
