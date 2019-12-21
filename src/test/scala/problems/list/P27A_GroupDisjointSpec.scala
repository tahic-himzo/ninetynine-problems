package problems.list

import org.scalatest._

class P27A_GroupDisjointSpec extends WordSpec with Matchers {
  "P27A_GroupDisjoint" should {
    "generate all disjoint groups of 2,3,4 possible" in {
      val result = P27A_GroupDisjoint.execute(List(1, 2, 3, 4, 5, 6, 7, 8, 9))

      //Amount of possible combinations is 9C2 * 7C3 * 4C4 = 1260
      result.length shouldEqual 1260
      Inspectors.forAll(result)(
        list => {
          list.length shouldEqual 3
          list match {
            case List(a, b, c) =>
              a.length shouldEqual 2
              b.length shouldEqual 3
              c.length shouldEqual 4
          }
          val flatList = list.flatten
          flatList.length shouldEqual 9
          flatList.toSet shouldEqual Set(1, 2, 3, 4, 5, 6, 7, 8, 9)
        }
      )
    }
  }
}
