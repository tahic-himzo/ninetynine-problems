package problems.list

import org.scalatest._

import scala.util.{Failure, Try}

class P23_SampleAListSpec extends WordSpec with Matchers {
  "P23_SampleAList" should {
    "extract n items of a list at random" in {
      testLengthAndDataIntersection(List(1, 2, 3, 4, 5, 6), 3)
      testLengthAndDataIntersection(List(1, 2, 3), 3)
      testLengthAndDataIntersection(List(1, 2), 1)
      testLengthAndDataIntersection(List(1, 2, 3, 4, 5, 6), 0)
    }

    "fail if n is negative" in {
      Try(P23_SampleAList.execute(-1, List(1, 2, 3))) shouldBe a[Failure[_]]
    }

    "fail if n is bigger than list length" in {
      Try(P23_SampleAList.execute(4, List(1, 2, 3))) shouldBe a[Failure[_]]
    }

    def testLengthAndDataIntersection(input: List[Int], n: Int): Assertion = {
      val output = P23_SampleAList.execute(n, input)
      output.length shouldEqual n
      Inspectors.forAll(output)(input.contains(_) shouldEqual true)
    }
  }
}
