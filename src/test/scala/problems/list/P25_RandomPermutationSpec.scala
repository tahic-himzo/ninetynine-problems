package problems.list

import org.scalatest._

class P25_RandomPermutationSpec extends WordSpec with Matchers {
  "P25_RandomPermutation" should {
    "create a random permutation of a list" in {
      testLengthAndDataIntersection(List(1, 2, 3, 4, 5, 6))
      testLengthAndDataIntersection(List(1, 2, 3))
      testLengthAndDataIntersection(List(1, 2))
      testLengthAndDataIntersection(List(1))
      testLengthAndDataIntersection(List())
    }

    def testLengthAndDataIntersection(input: List[Int]): Assertion = {
      val output = P25_RandomPermutation.execute(input)
      output.length shouldEqual input.length
      Inspectors.forAll(output)(input.contains(_) shouldEqual true)
    }
  }
}
