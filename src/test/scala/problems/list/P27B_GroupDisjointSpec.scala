package problems.list

import org.scalatest._

class P27B_GroupDisjointSpec extends WordSpec with Matchers {
  "P27B_GroupDisjoint" should {
    "generate all disjoint groups of a given grouping possible" in {
      val result = P27B_GroupDisjoint.execute(List(2, 3, 4), List(1, 2, 3, 4, 5, 6, 7, 8, 9))

      //Amount of possible combinations is 9C2 * 7C3 * 4C4 = 1260
      result.length shouldEqual 1260
      Inspectors.forAll(result)(
        list => {
          list.length shouldEqual 3

          val flatList = list.flatten
          flatList.length shouldEqual 9
          flatList.toSet shouldEqual Set(1, 2, 3, 4, 5, 6, 7, 8, 9)
        }
      )
    }
  }
}
