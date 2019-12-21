package problems.list

import org.scalatest._

class P19_RotateNSpec extends WordSpec with Matchers {
  "P19_RotateN" should {
    "shift all items in a list to the left by a given index" in {
      P19_RotateN.execute(3, List(1, 1, 2, 3, 5, 8, 10, 15)) shouldEqual List(3, 5, 8, 10, 15, 1, 1, 2)
      P19_RotateN.execute(-3, List(1, 1, 2, 3, 5, 8, 10, 15)) shouldEqual List(8, 10, 15, 1, 1, 2, 3, 5)
      P19_RotateN.execute(2, List(1, 1, 2, 3, 5, 8)) shouldEqual List(2, 3, 5, 8, 1, 1)
      P19_RotateN.execute(2, List(1, 2)) shouldEqual List(1, 2)
      P19_RotateN.execute(3, List(1, 2)) shouldEqual List(2, 1)
      P19_RotateN.execute(0, List(1, 2, 3)) shouldEqual List(1, 2, 3)
      P19_RotateN.execute(2, List()) shouldEqual List()
    }
  }
}
