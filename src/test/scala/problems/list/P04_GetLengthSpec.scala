package problems.list

import org.scalatest._

class P04_GetLengthSpec extends WordSpec with Matchers {
  "P04_GetLength" should {
    "return the length of a list" in {
      P04_GetLength.execute(List(1, 1, 2, 3, 5, 8)) shouldEqual 6
      P04_GetLength.execute(List(1, 2)) shouldEqual 2
    }
  }
}
